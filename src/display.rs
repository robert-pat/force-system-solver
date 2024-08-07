use macroquad::prelude as mq;
use std::collections::HashMap;

use crate::parsing::{AppliedLoad, Support, Truss2D};
use crate::solver::{Direction2D, Point2D, SolverID, VectorComponent};

/// Draw the provided truss and then exit the program.
/// This will leave the window open & interactable
/// but will exit the program once the window is closed.
///
/// This function should only be called when the program is otherwise ready to exit
pub fn display_truss_and_exit(truss: Truss2D, name: Option<&str>) -> ! {
    let name = String::from("Truss Visualization: ") + name.unwrap_or("");

    // This function is not actually intended to be called by user code; the library expects:
    // #[macroquad::main("label)]
    // async fn main() {}
    // which becomes this call, but I have a lot more code to run before this, so we abuse the
    // library (& DON'T UPDATE THE CRATE WITHOUT CHECKING THIS)
    macroquad::Window::new(&name, init_display_truss(truss));
    eprintln!("Exiting!");

    std::process::exit(0);
}

#[derive(Clone, Copy, Debug)]
struct TrussDrawSettings {
    screen_area: ScreenArea,
    point_width: f32,
    point_height: f32,
    member_thickness: f32,
    load_mag_scalar: f32,
    load_thickness: f32,
    load_arrow_scale: f32,
    pin_scale: f32,
    roller_scale: f32,
}
impl TrussDrawSettings {
    const fn default_with(area: ScreenArea) -> Self {
        TrussDrawSettings {
            screen_area: area,
            point_width: 18_f32,
            point_height: 18_f32,
            member_thickness: 9.5_f32,
            load_mag_scalar: 1_f32,
            load_thickness: 5_f32,
            load_arrow_scale: 10_f32,
            pin_scale: 6.5_f32,
            roller_scale: 10_f32,
        }
    }
}

/// Draws the given truss to the screen with macroquad.
fn draw_truss(truss: &Truss2D, settings: TrussDrawSettings) {
    mq::clear_background(mq::BEIGE);
    let coordinate_map = make_coord_map(truss, settings.screen_area);

    for point in truss.points.values() {
        let (width, height) = (settings.point_width, settings.point_height);
        let (x, y) = coordinate_map(point.pos());
        mq::draw_rectangle(x - 0.5 * width, y - 0.5 * height, width, height, mq::BLACK);
    }

    for (p1, p2) in truss.connections.iter() {
        let (x1, y1) = coordinate_map(truss.points.get(p1).unwrap().pos());
        let (x2, y2) = coordinate_map(truss.points.get(p2).unwrap().pos());

        mq::draw_line(x1, y1, x2, y2, settings.member_thickness, mq::BLACK);
    }

    for load in truss.loads.values() {
        let (x, y) = coordinate_map(truss.points.get(&load.at).unwrap().pos());
        // Invert the y-dir for screen space conversion (coordinate_map already does for pos)
        let (x_dir, y_dir) = (load.dir.x as f32, -load.dir.y as f32);
        let mag = load.mag.magnitude().unwrap() as f32;
        let (end_x, end_y) = (
            x + mag * x_dir * settings.load_mag_scalar,
            y + mag * y_dir * settings.load_mag_scalar,
        );
        mq::draw_line(x, y, end_x, end_y, settings.load_thickness, mq::BROWN);

        // vector from origin to tip of arrow / end of line
        let start = mq::vec2(end_x, end_y);
        let tri = triangle_with_rotation(load.dir, settings.load_arrow_scale);

        mq::draw_triangle(start + tri[0], start + tri[1], start + tri[2], mq::BROWN);
    }

    for support in truss.supports.values() {
        let (x, y) = coordinate_map(truss.points.get(&support.at()).unwrap().pos());
        match support {
            Support::Pin { .. } => {
                mq::draw_circle(x, y, settings.pin_scale, mq::BLUE);
            }
            Support::Roller { dir, .. } => {
                let start = mq::vec2(x, y);
                let tri = triangle_with_rotation(*dir, settings.roller_scale);
                mq::draw_triangle(start + tri[0], start + tri[1], start + tri[2], mq::BLUE);
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TextDrawSettings {
    screen_area: ScreenArea,
    font_size: f32,
    font_color: mq::Color,
    text_offset: (f32, f32),
}
impl TextDrawSettings {
    const fn default_with(area: ScreenArea) -> Self {
        TextDrawSettings {
            screen_area: area,
            font_size: 32f32,
            font_color: mq::WHITE,
            text_offset: (2.0, -1.5),
        }
    }
}

/// Draws labels for each point onto the screen. This uses the names from the truss's names map.
fn draw_labels(truss: &Truss2D, settings: TextDrawSettings) {
    let to_screen_space = make_coord_map(truss, settings.screen_area);

    for (id, point) in truss.points.iter() {
        let (x, y) = {
            let p = to_screen_space(point.pos());
            (p.0 + settings.text_offset.0, p.1 + settings.text_offset.1)
        };
        let name = match truss.names.get(id) {
            Some(i) => i.as_str(),
            None => "(unnamed)",
        };

        mq::draw_text(name, x, y, settings.font_size, settings.font_color);
    }
}

/// The default amount of padding around the edge of the window. This is used when drawing the truss
/// to take up the full screen.
const DEFAULT_PADDING: f32 = 60_f32;

/// Begin the control loop for drawing the truss to the screen & handling inputs.
async fn init_display_truss(truss: Truss2D) {
    loop {
        let area = {
            let extents = (mq::screen_width(), mq::screen_height());
            ScreenArea::with_padding(extents, DEFAULT_PADDING)
        };
        let truss_settings = TrussDrawSettings::default_with(area);
        let draw_settings = TextDrawSettings::default_with(area);

        draw_truss(&truss, truss_settings);
        draw_labels(&truss, draw_settings);

        if mq::is_key_pressed(mq::KeyCode::Escape) {
            break;
        }
        mq::next_frame().await
    }
}

/// Returns the three vertices of a unit triangle, scaled by scale and rotated to point in the
/// same direction as dir. The vertices are wound clockwise.
///
/// NOTE: this function already accounts for the conversion to screen space, it will handle flipping
/// the y-component automatically so that the drawn triangle appears as expected.
fn triangle_with_rotation(dir: Direction2D, scale: f32) -> [mq::Vec2; 3] {
    let rotation = {
        let test = mq::vec3(0.0, 1.0, 0.0); // 3D bc we need to cross them later
        let dir = mq::vec3(dir.x as f32, -dir.y as f32, 0.0);
        let cos = test.angle_between(dir);
        let sign = test.cross(dir).z.signum(); // tells us which 'side' the other vector is on
        mq::Vec2::from_angle(cos * sign)
    };
    // rotate the unit triangle by the amount we calculated
    [
        rotation.rotate(mq::vec2(0.0, 1.0).normalize()) * scale,
        rotation.rotate(mq::vec2(1.0, -1.0).normalize()) * scale,
        rotation.rotate(mq::vec2(-1.0, -1.0).normalize()) * scale,
    ]
}

/// A rectangular area of the window, measured with (0, 0) in the top right. The positive x-axis
/// extends left and the positive y-axis extends down.
///
/// Used to specify where on the screen things should be drawn in, i.e. what chunk of the window
/// a feature should be drawn in.
#[derive(Copy, Clone, Debug)]
struct ScreenArea {
    x_min: f32,
    x_max: f32,
    y_min: f32,
    y_max: f32,
}
impl ScreenArea {
    fn with_padding(extents: (f32, f32), padding: f32) -> Self {
        ScreenArea {
            x_min: padding,
            y_min: padding,
            x_max: extents.0 - padding,
            y_max: extents.1 - padding,
        }
    }
    fn width(&self) -> f32 {
        self.x_max - self.x_min
    }
    fn height(&self) -> f32 {
        self.y_max - self.y_min
    }
    fn from_truss(t: &Truss2D) -> Self {
        let (x_min, x_max) = t.points.values().map(|p| p.pos().0).fold(
            (f64::INFINITY, f64::NEG_INFINITY),
            |(x_min, x_max), x| {
                (
                    if x < x_min { x } else { x_min },
                    if x > x_max { x } else { x_max },
                )
            },
        );
        let (y_min, y_max) = t.points.values().map(|p| p.pos().1).fold(
            (f64::INFINITY, f64::NEG_INFINITY),
            |(y_min, y_max), y| {
                (
                    if y < y_min { y } else { y_min },
                    if y > y_max { y } else { y_max },
                )
            },
        );
        ScreenArea {
            x_min: x_min as f32,
            x_max: x_max as f32,
            y_min: y_min as f32,
            y_max: y_max as f32,
        }
    }
}

/// Create a closure to map between the truss coordinate space and screen space. This is a simple
/// map & is computed once, so any change to either the truss or screen dimensions will require a
/// new closure to be created.
///
/// This map will have the same scaling for the two axis, so that shapes are preserved. Note that
/// this might mean some trusses get very squashed along one axis if they are very different in size.
/// The [make_coord_map_independent] function has the same functionality but w/ independent axes.
fn make_coord_map(truss: &Truss2D, area: ScreenArea) -> impl Fn((f64, f64)) -> (f32, f32) {
    // Find the bounds for our input ranges
    let source = ScreenArea::from_truss(truss);
    let scale = (area.width() / source.width()).min(area.height() / source.height());

    move |(x, y)| {
        let x = (x as f32 - source.x_min) * scale + area.x_min;
        let y = (y as f32 - source.y_min) * scale + area.y_min;

        (x, area.y_max - y + area.y_min)
    }
}

/// Returns a closure that will map coordinates from the truss onto the available screen space, with
/// the specified padding. This is a simple linear map, so if the truss's points or screen dimensions
/// change, the map will need to be recalculated.
///
/// This map takes will also flip the y-coordinate, so that a larger y-input results in a coordinate
/// drawn closer to the top of the screen. Macroquad has the +y-axis pointing downward, so this
/// means very positive y-values get mapped close to 0 & vise-versa
#[allow(dead_code)]
fn make_coord_map_independent(
    truss: &Truss2D,
    screen_max: (f32, f32),
    padding: f32,
) -> impl Fn((f64, f64)) -> (f32, f32) {
    // Find the bounds for our input ranges
    let source = ScreenArea::from_truss(truss);
    let (x_min, x_max) = (source.x_min, source.x_max);
    let (y_min, y_max) = (source.y_min, source.y_max);

    // Find the bounds for our output ranges
    let x_range = (padding, screen_max.0 - padding);
    let y_range = (padding, screen_max.1 - padding);

    // Map the input ranges to the output ranges (and convert f64 -> f32)
    // https://stackoverflow.com/questions/5731863/mapping-a-numeric-range-onto-another
    // NOTE: the y-coordinate is reversed bc screen space has (0, 0) in the top left
    move |(x, y)| {
        let x = (x as f32 - x_min) * (x_range.1 - x_range.0) / (x_max - x_min) + x_range.0;
        let y = (y as f32 - y_min) * (y_range.1 - y_range.0) / (y_max - y_min) + y_range.0;
        (x, screen_max.1 - y)
    }
}

/// Returns a truss filled with testing values for drawing. This truss is physically impossible and
/// not in equilibrium. It can not be solved
pub(crate) fn get_sample_truss() -> Truss2D {
    let points: HashMap<SolverID, Point2D> = [
        ("A", (0, 0)),
        ("B", (100, 100)),
        ("C", (23, 89)),
        ("D", (7, 13)),
        ("E", (30, 67)),
        ("F", (77, 16)),
    ]
    .into_iter()
    .map(|(n, (x, y))| {
        let n = SolverID::new(n);
        (n, Point2D::cartesian(n, x as f64, y as f64))
    })
    .collect();
    let connections = vec![("A", "B"), ("A", "C"), ("A", "D"), ("E", "F"), ("C", "B")]
        .into_iter()
        .map(|(a, b)| (SolverID::new(a), SolverID::new(b)))
        .collect();
    // omega illegal truss, is not being constructed fully rigorously
    let loads: HashMap<SolverID, AppliedLoad> = [
        ("A", 25, 12),
        ("B", 25, 300),
        ("B", 25, 180),
        ("D", 25, 225),
        ("E", 25, 75),
    ]
    .into_iter()
    .map(|(a, b, c)| AppliedLoad {
        at: SolverID::new(a),
        id: SolverID::new("_"),
        dir: Direction2D::from_degrees(c as f64),
        mag: VectorComponent::KnownExactly(b as f64),
    })
    .map(|l| (l.at, l))
    .collect();
    let mut supports = HashMap::new();
    supports.insert(
        SolverID::new("F"),
        Support::Pin {
            at: SolverID::new("F"),
        },
    );
    supports.insert(
        SolverID::new("C"),
        Support::Roller {
            at: SolverID::new("C"),
            dir: Direction2D::from_degrees(70.0),
        },
    );

    let names: HashMap<SolverID, String> = [
        (SolverID::new("A"), String::from("A")),
        (SolverID::new("B"), String::from("B")),
        (SolverID::new("C"), String::from("C")),
        (SolverID::new("D"), String::from("D")),
        (SolverID::new("E"), String::from("E")),
        (SolverID::new("F"), String::from("F")),
    ]
    .into_iter()
    .collect();
    Truss2D {
        points,
        connections,
        loads,
        supports,
        names,
    }
}
