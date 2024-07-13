use macroquad::prelude as mq;
use std::collections::HashMap;

use crate::parsing::{AppliedLoad, Support, Truss2D};
use crate::solver::{Direction2D, Point2D, SolverID, VectorComponent};

/// Draw the provided truss and then exit the program.
/// This will leave the window open & interactable
/// but will exit the program once the window is closed.
///
/// This function should only be called when the program is otherwise ready to exit
pub fn display_truss_and_exit(truss: Truss2D) -> ! {
    // This function is not actually intended to be called by user code; the library expects:
    // #[macroquad::main("label)]
    // async fn main() {}
    // which becomes this call, but I have a lot more code to run before this, so we abuse the
    // library (& DON'T UPDATE THE CRATE WITHOUT CHECKING THIS)
    macroquad::Window::new("Truss Visualization", init_display_truss(truss));
    eprintln!("Exiting!");

    std::process::exit(0);
}

/// Draws the given truss to the screen with macroquad.
fn draw_truss(truss: &Truss2D) {
    const SCREEN_PADDING: f32 = 60_f32;
    let screen_bounds = (mq::screen_width(), mq::screen_height());
    mq::clear_background(mq::BEIGE);
    let coordinate_map = make_coordinate_map(truss, screen_bounds, SCREEN_PADDING);

    for point in truss.points.values() {
        const POINT_WIDTH: f32 = 18_f32;
        const POINT_HEIGHT: f32 = 18_f32;

        let (x, y) = coordinate_map(point.pos());
        mq::draw_rectangle(
            x - 0.5 * POINT_WIDTH,
            y - 0.5 * POINT_HEIGHT,
            POINT_WIDTH,
            POINT_HEIGHT,
            mq::BLACK,
        );
    }

    const MEMBER_THICKNESS: f32 = 9.5_f32;
    for (p1, p2) in truss.connections.iter() {
        let (x1, y1) = coordinate_map(truss.points.get(p1).unwrap().pos());
        let (x2, y2) = coordinate_map(truss.points.get(p2).unwrap().pos());

        mq::draw_line(x1, y1, x2, y2, MEMBER_THICKNESS, mq::BLACK);
    }

    const LOAD_MAG_SCALAR: f32 = 1_f32; // eventually will need to be non-const
    const LOAD_THICKNESS: f32 = 5_f32;
    const LOAD_ARROW_SCALE: f32 = 10_f32;
    for load in truss.loads.values() {
        let (x, y) = coordinate_map(truss.points.get(&load.at).unwrap().pos());
        // Invert the y-dir for screen space conversion (coordinate_map already does for pos)
        let (x_dir, y_dir) = (load.dir.x as f32, -load.dir.y as f32);
        let mag = load.mag.magnitude().unwrap() as f32;
        let (end_x, end_y) = (
            x + mag * x_dir * LOAD_MAG_SCALAR,
            y + mag * y_dir * LOAD_MAG_SCALAR,
        );
        mq::draw_line(x, y, end_x, end_y, LOAD_THICKNESS, mq::BROWN);

        // vector from origin to tip of arrow / end of line
        let start = mq::vec2(end_x, end_y);
        let tri = triangle_with_rotation(load.dir, LOAD_ARROW_SCALE);
        
        mq::draw_triangle(start + tri[0], start + tri[1], start + tri[2], mq::BROWN);
    }

    const PIN_SCALE: f32 = 6.5_f32;
    const ROLLER_SCALE: f32 = 10_f32;
    for support in truss.supports.values() {
        let (x, y) = coordinate_map(truss.points.get(&support.at()).unwrap().pos());
        match support {
            Support::Pin { .. } => {
                mq::draw_circle(x, y, PIN_SCALE, mq::BLUE);
            }
            Support::Roller { dir, .. } => {
                let start = mq::vec2(x, y);
                let tri = triangle_with_rotation(*dir, ROLLER_SCALE);
                mq::draw_triangle(start + tri[0], start + tri[1], start + tri[2], mq::BLUE);
            }
        }
    }
}

/// Begin the control loop for drawing the truss to the screen & handling inputs.
async fn init_display_truss(truss: Truss2D) {
    loop {
        draw_truss(&truss);

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

/// Returns a closure that will map coordinates from the truss onto the available screen space, with
/// the specified padding. This is a simple linear map, so if the truss's points or screen dimensions
/// change, the map will need to be recalculated.
///
/// This map takes will also flip the y-coordinate, so that a larger y-input results in a coordinate
/// drawn closer to the top of the screen. Macroquad has the +y-axis pointing downward, so this
/// means very positive y-values get mapped close to 0 & vise-versa
fn make_coordinate_map(
    truss: &Truss2D,
    screen_max: (f32, f32),
    padding: f32,
) -> impl Fn((f64, f64)) -> (f32, f32) {
    // TODO: make this return a consistent scaling for both x and y directions

    // Find the bounds for our input ranges
    let (x_min, x_max) = truss
        .points
        .values()
        .map(|p| p.pos().0)
        // take the smallest and largest x pos in the position range
        .fold((f64::INFINITY, f64::NEG_INFINITY), |(x_min, x_max), x| {
            (
                if x < x_min { x } else { x_min },
                if x > x_max { x } else { x_max },
            )
        });
    let (y_min, y_max) = truss
        .points
        .values()
        .map(|p| p.pos().1)
        // take the smallest and largest y pos in the position range
        .fold((f64::INFINITY, f64::NEG_INFINITY), |(y_min, y_max), y| {
            (
                if y < y_min { y } else { y_min },
                if y > y_max { y } else { y_max },
            )
        });
    // Find the bounds for our output ranges
    let x_range = (padding as f64, (screen_max.0 - padding) as f64);
    let y_range = (padding as f64, (screen_max.1 - padding) as f64);

    // Map the input ranges to the output ranges (and convert f64 -> f32)
    // https://stackoverflow.com/questions/5731863/mapping-a-numeric-range-onto-another
    // NOTE: the y-coordinate is reversed bc screen space has (0, 0) in the top left
    move |(x, y)| {
        let x = (x - x_min) * (x_range.1 - x_range.0) / (x_max - x_min) + x_range.0;
        let y = (y - y_min) * (y_range.1 - y_range.0) / (y_max - y_min) + y_range.0;
        (x as f32, screen_max.1 - (y as f32))
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
        .map(|(a, b, c)|{
            AppliedLoad {
                at: SolverID::new(a),
                id: SolverID::new("_"),
                dir: Direction2D::from_degrees(c as f64),
                mag: VectorComponent::KnownExactly(b as f64),
            }
        })
        .map(|l| (l.at, l))
        .collect();
    let mut supports = HashMap::new();
    supports.insert(SolverID::new("F"), Support::Pin {at: SolverID::new("F")});
    supports.insert(SolverID::new("C"), Support::Roller {
        at: SolverID::new("C"),
        dir: Direction2D::from_degrees(70.0),
    });

    Truss2D {
        points,
        connections,
        loads,
        supports,
        names: HashMap::new(),
    }
}
