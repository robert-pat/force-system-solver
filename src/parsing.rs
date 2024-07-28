use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};
use std::fmt::Formatter;
use std::io::Write;

use itertools::Itertools;
use nalgebra as na;
use toml::{Table, Value};

use crate::solver::{Direction2D, Force2D, Point2D, SolverID, TrussJoint2D, VectorComponent};

pub(crate) struct ProblemInformation {
    pub(crate) name: String,
    pub(crate) debug: DebugInfo,
    pub(crate) file_write: bool,
}
/// Grabs important settings information from a TOML file. This function will not error.
pub(crate) fn get_problem_information(problem: &Table) -> ProblemInformation {
    let table = problem;
    let name = match table.get("name") {
        Some(Value::String(s)) => s.clone(),
        Some(value) => value.to_string(),
        None => String::from("Unnamed problem"),
    };
    let debug_enabled = match table.get("debug") {
        Some(Value::Boolean(b)) => *b,
        _ => false,
    };
    let file_write = match table.get("write-file") {
        Some(Value::Boolean(b)) => *b,
        _ => false,
    };

    let output: Box<dyn Write> = if file_write {
        let file = std::fs::OpenOptions::new()
            .append(true)
            .write(true)
            .create(true)
            .open(format!("answer-{}", name))
            .expect("Unable to open new file to write output to");
        Box::new(file)
    } else {
        Box::new(std::io::stdout())
    };

    ProblemInformation {
        name,
        debug: DebugInfo {
            enabled: debug_enabled,
            output,
        },
        file_write,
    }
}

pub(crate) struct DebugInfo {
    pub(crate) enabled: bool,
    pub(crate) output: Box<dyn Write>,
}
impl DebugInfo {
    #[allow(unused)]
    pub(crate) fn empty() -> Self {
        DebugInfo {
            enabled: false,
            output: Box::new(EmptyWriter()),
        }
    }
    #[allow(unused)]
    pub(crate) fn stderr(enabled: bool) -> Self {
        DebugInfo {
            enabled,
            output: Box::new(std::io::stderr()),
        }
    }
    /// Display the given matrix, limited to whatever the solver::solve_truss() generates.
    ///
    /// Will print the name and then each row of the matrix in order. The size of the matrix (rows
    /// and columns) is also logged for ease of reading.
    #[allow(unused)] // rn idgaf if writing to stdout fails
    pub(crate) fn display_matrix(&mut self, m: &na::OMatrix<f64, na::Dyn, na::Dyn>, name: &str) {
        writeln!(
            self.output,
            "{} [{} rows by {} columns]:",
            name,
            m.nrows(),
            m.ncols()
        );
        for row in m.row_iter() {
            write!(self.output, "[ ");
            for v in row.iter() {
                write!(self.output, "{}, ", *v);
            }
            writeln!(self.output, "]");
        }
    }
    pub(crate) fn display_joints(&mut self, joints: &[TrussJoint2D]) -> Result<(), ()> {
        writeln!(self.output, "Created Joints:").map_err(|_| ())?;
        for joint in joints {
            writeln!(self.output, "{joint}").map_err(|_| ())?
        }
        writeln!(self.output).map_err(|_| ())
    }
    pub(crate) fn display_names<'a>(
        &mut self,
        names: impl Iterator<Item = (&'a SolverID, &'a String)>,
    ) -> Result<(), ()> {
        writeln!(self.output, "Names for solver items:").map_err(|_| ())?;
        for (id, name) in names {
            writeln!(self.output, "Item {id} is {name}").map_err(|_| ())?;
        }
        writeln!(self.output).map_err(|_| ())
    }
}
#[allow(dead_code)]
struct EmptyWriter();
impl Write for EmptyWriter {
    #[allow(unused)]
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        Ok(0)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
/// Converts a toml value into an array of toml values or panics
macro_rules! array_me {
    ($value: expr) => {
        if let toml::Value::Array(a) = $value {
            a
        } else {
            eprintln!("Error expected a TOML array, but got {:?} instead", $value);
            eprintln!("Make sure you've declared everything in arrays (with \'[\' and \']\').");
            panic!("This might be a programming error!");
        }
    };
}

/// Takes in a toml::Value::Array and parses it into an array of Point2D for the
/// solver to use. The function signature doesn't capture this, but the Value passed in
/// must be the Value::Array(_) variant, or the function will panic.
///
/// Also updates a map between SolverIDs and names with the names of all points it parses.
pub(crate) fn parse_points(
    point_declarations: &Value,
    names: &mut BTreeMap<SolverID, String>,
    debug: &mut DebugInfo,
) -> BTreeMap<SolverID, Point2D> {
    // For reference, declarations are: Name, System, <number>, <number>
    let declarations = array_me!(point_declarations);
    let mut points: BTreeMap<SolverID, Point2D> = BTreeMap::new();

    for entry in declarations {
        let mut tokens = match entry {
            Value::Array(a) if a.is_empty() => {
                eprintln!("WARNING: Saw empty point!");
                continue;
            }
            Value::Array(a) if [2, 4].contains(&a.len()) => a.iter(),
            Value::Array(a) => {
                eprintln!("Point \'{}\' has the wrong number of values declared", a[0]);
                panic!("Can't parse point \'{}\'", a[0]);
            }
            _a => panic!("Error: points should be declared with a toml array! Saw {_a:?}"),
        };

        // Guaranteed at least 2 Some(_) from tokens
        let (id, point_name) = match tokens.next().unwrap() {
            Value::String(s) => (SolverID::new(s.as_str()), s.as_str()),
            other => panic!("Expected a point name (text), but got \'{:?}\'", other),
        };
        let system = match tokens.next().unwrap() {
            Value::String(s) => s.as_str(),
            _a => panic!("Point {point_name} has an invalid coordinate system: {_a:?}!"),
        };
        let c1 = match tokens.next() {
            Some(Value::Integer(i)) => *i as f64,
            Some(Value::Float(f)) => *f,
            _ => f64::NAN,
        };
        let c2 = match tokens.next() {
            Some(Value::Integer(i)) => *i as f64,
            Some(Value::Float(f)) => *f,
            _ => f64::NAN,
        };

        let point = match system {
            "Origin" => Point2D::origin(id),
            "Cartesian" => Point2D::cartesian(id, c1, c2),
            "Polar" => Point2D::polar(id, c1, c2),
            _a => {
                eprintln!("Point \'{point_name}\' declared with invalid coordinate system: {_a:?}");
                panic!("Point coordinate system must be: Origin, Cartesian, or Polar");
            }
        };

        // since we introduce the possibility that a coord maybe be NaN or empty
        assert!(
            point.is_valid(),
            "Point \'{point_name}\' declared with non-number coordinates"
        );
        if debug.enabled {
            writeln!(debug.output, "Parsed point {point_name} to {point}").unwrap();
        }

        // Check for duplicate points
        if points.insert(id, point).is_some() {
            eprintln!("Warning! Duplicate point name declared: \'{point_name}\'!");
            panic!("Comment out the extra point with \'#\' to save it for later");
        }
        names.insert(id, point_name.to_string());
    }
    points
}

/// Parses the applied loads in a problem. The toml_array argument must be the toml::Value::Array(_)
/// discriminant, as the name suggests. Like other functions of this type, it takes in map to store
/// human-readable names for each force it parses.
pub(crate) fn parse_loads(
    load_declarations: &Value,
    points: &BTreeMap<SolverID, Point2D>,
    names: &mut BTreeMap<SolverID, String>,
) -> Vec<Force2D> {
    let raw_forces = array_me!(load_declarations);
    let mut forces: Vec<Force2D> = Vec::new();
    let mut unique_id = 0usize;

    for raw_force in raw_forces {
        let mut tokens = match raw_force {
            Value::Array(a) if a.is_empty() => {
                eprintln!("WARING: Empty applied load defined!");
                continue;
            }
            Value::Array(a) if [3, 4].contains(&a.len()) => a.iter(),
            Value::Array(a) => {
                eprintln!(
                    "Load acting at \'{}\' has the wrong number of values declared!",
                    a[0]
                );
                panic!("Couldn't parse the load acting at \'{}\'", a[0]);
            }
            _a => panic!("Applied loads must be toml arrays, not: {_a:?}"),
        };

        // Gaunted at least 3 Some(_) from tokens
        let (point_id, point_name) = match tokens.next().unwrap() {
            Value::String(s) => (SolverID::new(s.as_str()), s.as_str()),
            _a => panic!("Expected the name of a point (text), but got \'{_a:?}\'"),
        };
        let magnitude = match tokens.next().unwrap() {
            Value::Float(f) => *f,
            Value::Integer(i) => *i as f64,
            _a => panic!("Load \'{point_name}\' has improperly defined magnitude: \'{_a:?}\'"),
        };

        let direction = match tokens.next().unwrap() {
            Value::String(s) if s.as_str() == "Up" => Direction2D::from_degrees(90.0),
            Value::String(s) if s.as_str() == "Down" => Direction2D::from_degrees(270.0),
            Value::String(s) if s.as_str() == "Left" => Direction2D::from_degrees(180.0),
            Value::String(s) if s.as_str() == "Right" => Direction2D::from_degrees(0.0),
            Value::String(s) if s.as_str() == "Polar" => match tokens.next() {
                Some(Value::Float(f)) => Direction2D::from_degrees(*f),
                Some(Value::Integer(i)) => Direction2D::from_degrees(*i as f64),
                Some(_o) => {
                    panic!("Angle for load at \'{point_name}\' must be degrees, not \'{_o:?}\'")
                }
                None => panic!("No direction provided for load \'{point_name}\'!"),
            },
            _a => {
                eprintln!("Invalid direction for applied force at \'{point_name}\': {_a:?}");
                panic!("Applied forces must be Up, Down, Left, Right, or Polar (with angle)");
            }
        };

        // We have to manually give each on of these a unique name bc you can have multiple loads
        // applied at the same point, but the solver needs each one to have a unique ID.
        // Could combine all of them to one, but that would be more complex I think?
        let load_name_unique = format!("Load {unique_id} at {point_name}");
        unique_id += 1;
        let point = match points.get(&point_id) {
            Some(p) => p.clone(),
            None => {
                panic!("Loads must be attached to a valid point, \'{point_name}\' does not exist")
            }
        };
        let force = Force2D::new(
            SolverID::new(&load_name_unique),
            point,
            direction,
            VectorComponent::KnownExactly(magnitude),
        );

        names.insert(SolverID::new(&load_name_unique), load_name_unique);
        forces.push(force);
    }
    forces
}

/// Does the same thing as the `parse_loads(...)` function, but with the support reactions.
pub(crate) fn generate_support_reactions(
    array: &Value,
    points: &BTreeMap<SolverID, Point2D>,
    name_map: &mut BTreeMap<SolverID, String>,
) -> Vec<Force2D> {
    let raw_supports = array_me!(array);
    let mut support_reactions: Vec<Force2D> = Vec::new();

    for raw_support in raw_supports {
        let mut tokens = array_me!(raw_support).iter();
        #[allow(unused)] // used in format strings
        let (point_id, name) = match tokens.next() {
            Some(Value::String(s)) => (SolverID::new(s.as_str()), s.as_str()),
            Some(other) => panic!(
                "Supports must be attached at pre-defined points! Saw \'{:?}\'",
                other
            ),
            None => panic!("Supports can not be empty! Saw a support with nothing defined"),
        };
        let attached_point = match points.get(&point_id) {
            Some(p) => p,
            None => panic!(
                "Supports must be attached to an existing point! Point \'{name}\' does not exist!"
            ),
        };
        // these are just to reduce nesting flow control and matches
        enum Support {
            Roller,
            Pin,
        }
        enum Direction {
            Up,
            Down,
            Left,
            Right,
        }
        let support_type = match tokens.next() {
            Some(Value::String(s)) if s.as_str() == "Pin" => Support::Pin,
            Some(Value::String(s)) if s.as_str() == "Roller" => Support::Roller,
            Some(other) => panic!(
                "Supports at point \'{name}\'must be one of [Pin, Roller], saw \'{:?}\'",
                other
            ),
            None => panic!("Supports need a type! Support at point {name} has None!"),
        };
        match support_type {
            Support::Pin => {
                let nx = format!("Pin {}x{}", name, rand::random::<usize>());
                let idx = SolverID::new(&nx);
                name_map.insert(idx, nx);

                let ny = format!("Pin {}y{}", name, rand::random::<usize>());
                let idy = SolverID::new(&ny);
                name_map.insert(idy, ny);

                support_reactions.push(Force2D::new(
                    idx,
                    attached_point.clone(),
                    Direction2D::from_degrees(0f64),
                    VectorComponent::Unknown,
                ));
                support_reactions.push(Force2D::new(
                    idy,
                    attached_point.clone(),
                    Direction2D::from_degrees(90f64),
                    VectorComponent::Unknown,
                ));
            }
            Support::Roller => {
                let dir = match tokens.next() {
                    Some(Value::String(s)) if s.as_str() == "Up" => Direction::Up,
                    Some(Value::String(s)) if s.as_str() == "Down" => Direction::Down,
                    Some(Value::String(s)) if s.as_str() == "Left" => Direction::Left,
                    Some(Value::String(s)) if s.as_str() == "Right" => Direction::Right,
                    Some(other) => panic!("Rollers should have a direction [Up, Down, Left, Right]. Roller at point \'{name}\' has {:?}", other),
                    None => panic!("Rollers must have a direction! Roller at point \'{name}\' has none!"),
                };
                let unique_name = format!("Roller {}-{}", name, rand::random::<usize>());
                let id = SolverID::new(&unique_name);
                name_map.insert(id, unique_name);

                support_reactions.push(Force2D::new(
                    id,
                    attached_point.clone(),
                    match dir {
                        Direction::Up => Direction2D::from_degrees(90f64),
                        Direction::Down => Direction2D::from_degrees(270f64),
                        Direction::Left => Direction2D::from_degrees(0f64),
                        Direction::Right => Direction2D::from_degrees(180f64),
                    },
                    VectorComponent::KnownPositive,
                ));
            }
        };
    }
    support_reactions
}

/// Creates the forces from a structural member on each of the two end points of that member. These fores
/// have the same ID and are named from the two points that define them.
fn generate_internal_forces(
    member_declarations: &Value,
    points: &BTreeMap<SolverID, Point2D>,
    names: &mut BTreeMap<SolverID, String>,
) -> Vec<Force2D> {
    let mut internal_forces: Vec<Force2D> = Vec::new();
    let raw_members = array_me!(member_declarations);
    let mut members: Vec<(SolverID, SolverID)> = Vec::new();

    for raw_member in raw_members {
        let mut tokens = match raw_member {
            Value::Array(a) if a.is_empty() => {
                eprintln!("WARNING: Empty internal member declared!");
                continue;
            }
            Value::Array(a) if a.len() == 2 => a.iter(),
            Value::Array(_) => panic!("Internal member declared with incorrect number of points!"),
            _a => panic!("Internal members must be defined with a toml array! Saw {_a:?}"),
        };

        // To ensure that member AB has the same order at member BA, we sort.
        // The name map part is bad, but it will prob work for now
        let (name1, name2) = match (tokens.next().unwrap(), tokens.next().unwrap()) {
            (Value::String(s1), Value::String(s2)) => match s1.cmp(s2) {
                Ordering::Greater => (s1, s2),
                Ordering::Less => (s2, s1),
                Ordering::Equal => {eprintln!("Non-existent member declared! [{s1:?}, {s2:?}]"); continue;}
            }
            _a => panic!("Incorrect structural member definition! Expected \'point1, point2\', got \'{_a:?}\'")
        };
        let (id1, id2) = (SolverID::new(name1), SolverID::new(name2));
        let new_id = id1.concatenate(id2);

        // ideally this would only be done w/ the de-duped list, but I can't currently think of how
        names.insert(new_id, format!("Member {name1}<->{name2}"));
        members.push((id1, id2));
    }

    // this should be fine? bc the elements to remove should be identical so doesn't matter how
    // they're sorted ??
    members.sort();
    members.dedup();

    // actually create the forces:
    for (id1, id2) in members {
        let new_id = id1.concatenate(id2);
        let (p1, p2) = match (points.get(&id1), points.get(&id2)) {
            (Some(a), Some(b)) => (a, b),
            (Some(_), None) => panic!("Member declared from point that does not exist: {}", id2),
            (None, Some(_)) => panic!("Member declared from point that does not exist: {}", id1),
            _ => panic!("Member declared from points that do not exist: {id1}, {id2}"),
        };
        let (d1, d2) = (p1.direction_to(p2), p2.direction_to(p1));
        internal_forces.push(Force2D::new(
            new_id,
            p1.clone(),
            d1,
            VectorComponent::Unknown,
        ));
        internal_forces.push(Force2D::new(
            new_id,
            p2.clone(),
            d2,
            VectorComponent::Unknown,
        ));
    }
    internal_forces
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParsingError {
    InvalidTOMLFile(toml::de::Error),
    MissingPointsTable,
    MissingMembersTable,
    MissingLoadsTable,
    MissingSupportsTable,
}
impl From<toml::de::Error> for ParsingError {
    fn from(value: toml::de::Error) -> Self {
        ParsingError::InvalidTOMLFile(value)
    }
}

pub struct ParsedProblem {
    pub(crate) name_map: BTreeMap<SolverID, String>,
    pub(crate) joints: Vec<TrussJoint2D>,
}

/// Parse a TOML file into usable information for the solver and rest of the program to use.
/// The file must be the full file, read into a string.
pub(crate) fn parse_problem(
    file: String,
    debug: &mut DebugInfo,
) -> Result<ParsedProblem, ParsingError> {
    let toml_file = file.parse::<Table>()?;
    let mut names_record = BTreeMap::new();
    let points = {
        let raw = toml_file
            .get("points")
            .ok_or(ParsingError::MissingPointsTable)?;
        let p = parse_points(raw, &mut names_record, debug);
        if let Err(e) = validate_points(&p) {
            eprintln!("WARNING: {e}");
        }
        p
    };

    let internal_forces = {
        let i = toml_file
            .get("members")
            .ok_or(ParsingError::MissingMembersTable)?;
        generate_internal_forces(i, &points, &mut names_record)
    };
    let loads = {
        let l = toml_file
            .get("loads")
            .ok_or(ParsingError::MissingLoadsTable)?;
        parse_loads(l, &points, &mut names_record)
    };
    let support_reactions = {
        let s = toml_file
            .get("supports")
            .ok_or(ParsingError::MissingSupportsTable)?;
        generate_support_reactions(s, &points, &mut names_record)
    };

    let mut joints: BTreeMap<SolverID, TrussJoint2D> = points
        .keys()
        .map(|id| (*id, TrussJoint2D::empty(*id)))
        .collect();

    // Add all of the forces we've created onto their respective joints
    for force in loads
        .into_iter()
        .chain(internal_forces)
        .chain(support_reactions)
    {
        let joint = match joints.get_mut(&force.point_id()) {
            Some(j) => j,
            None => panic!("Force {:?}'s point ID did not match any joints!", force),
        };
        joint.add(force);
    }

    Ok(ParsedProblem {
        name_map: names_record,
        joints: joints.into_values().collect(),
    })
}

/* Working on proper validation for problems, might need to do this after problem is completed??
 * bc honestly, to might be a lot simpler / make more sense to do all the validation in the solver
 * with the completely parsed components / forces. Will still need to check that there's enough info
 * to do that then
 */
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum PointValidationError {
    PointOverLaps(SolverID, SolverID),
}
impl std::fmt::Display for PointValidationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PointValidationError::PointOverLaps(a, b) => {
                write!(f, "two points have very similar positions: {a} and {b} ")
            }
        }
    }
}

fn validate_points(points: &BTreeMap<SolverID, Point2D>) -> Result<(), PointValidationError> {
    // we know the points is all be normal & have no duplicate IDs because
    // the generating points function checks that
    let points: Vec<Point2D> = points.values().cloned().collect();
    for p in points.into_iter().permutations(2) {
        let (x1, y1) = p[0].pos();
        let (x2, y2) = p[1].pos();

        const CLOSE_THRESHOLD: f64 = 0.001f64;
        if (x1 - x2).abs() <= CLOSE_THRESHOLD && (y1 - y2).abs() <= CLOSE_THRESHOLD {
            return Err(PointValidationError::PointOverLaps(p[0].id(), p[1].id()));
        }
    }
    Ok(())
}

/* ---- Experiments Below Here! -----*/

/// Grabs the specified sub-table from the [Table] provided. This macro is effectively the same as
/// the included [Table::get()] method, but it returns a Result<_, [ConversionError]> instead of an
/// Option<_>
macro_rules! open_table {
    ($value:expr, $table:expr) => {{
        let a: &str = $table.into();
        match $value.get(a) {
            Some(v) => Ok(v),
            None => Err(ConversionError::NotATable(
                format!("missing table in toml file: ") + a,
            )),
        }
    }};
}
/// An external support at a joint in the truss; support reactions with known directions, but
/// unknown magnitudes. It may be a pin support or a roller.
#[derive(Clone, Debug)]
pub(crate) enum Support {
    Pin { at: SolverID },
    Roller { at: SolverID, dir: Direction2D },
}
impl Support {
    pub(crate) fn at(&self) -> SolverID {
        match self {
            Support::Pin { at } => *at,
            Support::Roller { at, dir: _dir } => *at,
        }
    }
}
/// An external load applied to the truss at a specific point. These forces have both known
/// directions and a known magnitude.
#[derive(Clone, Copy, Debug)]
pub(crate) struct AppliedLoad {
    pub(crate) id: SolverID,
    pub(crate) at: SolverID,
    pub(crate) dir: Direction2D,
    pub(crate) mag: VectorComponent,
}

/// Represents a two-dimensional truss in static equilibrium. Trusses are made up of structural
/// members connected at shared points, and supports connected to the structure at these joints.
/// Trusses constructed like this may be analyzed with the method of joints. The [Truss2D::condense]
/// method allows for this.
///
/// This struct holds all of the necessary information to display and solve a truss and
/// human-friendly names for each relevant truss component. It is guaranteed to be valid, with
/// all truss members, supports, and applied loads acting only at valid joints.
#[derive(Clone, Debug, Default)]
pub(crate) struct Truss2D {
    pub(crate) points: HashMap<SolverID, Point2D>,
    pub(crate) connections: Vec<(SolverID, SolverID)>,
    pub(crate) loads: HashMap<SolverID, AppliedLoad>,
    pub(crate) supports: HashMap<SolverID, Support>,
    pub(crate) names: HashMap<SolverID, String>,
}
/// An error encountered in the process of creating a new [Truss2D]. Can contain either
/// [ConversionError]s from unwrapping values from a toml file, or issues found when validating the
/// truss.
#[derive(Debug, Clone)]
pub(crate) enum TrussCreationError {
    Conversion(ConversionError),
    PointNonExistent(String),
    PointsOverlap(String),
}
impl std::fmt::Display for TrussCreationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TrussCreationError::Conversion(c) => write!(f, "{c}"),
            TrussCreationError::PointsOverlap(s) => write!(f, "{s}"),
            TrussCreationError::PointNonExistent(s) => write!(f, "{s}"),
        }
    }
}
impl From<ConversionError> for TrussCreationError {
    fn from(value: ConversionError) -> Self {
        TrussCreationError::Conversion(value)
    }
}
impl Truss2D {
    /// Try to turn the given [Table] into a [Truss2D]. The function will error on the first issue
    /// it encounters; the returned [TrussCreationError] has a message with details about what the
    /// problem was. Note empty definitions are ignored while duplicate ones are errors.
    pub(crate) fn new(t: &Table) -> Result<Self, TrussCreationError> {
        let mut names: HashMap<SolverID, String> = HashMap::new();

        let points = into_points(open_table!(&t, "points")?, &mut names)?;
        let connections = into_connections(open_table!(&t, "members")?, &mut names)?;
        let loads = into_loads(open_table!(&t, "loads")?, &mut names)?;
        let supports = into_supports(open_table!(&t, "supports")?, &mut names)?;

        // check to make sure everything is defined at a valid point
        for (a, b) in &connections {
            let combined = names.get(&a.concatenate(*b)).unwrap();
            if !points.contains_key(a) || !points.contains_key(b) {
                return Err(TrussCreationError::PointNonExistent(format!(
                    "{combined} is attached to a point that does not exist"
                )));
            }
        }
        if let Some(l) = loads.iter().find(|(_, l)| !points.contains_key(&l.at)) {
            let name = names.get(l.0).unwrap();
            return Err(TrussCreationError::PointNonExistent(format!(
                "{name} is attached to a point that does not exist"
            )));
        }
        if let Some(s) = supports.iter().find(|(_, s)| !points.contains_key(&s.at())) {
            let name = names.get(s.0).unwrap();
            return Err(TrussCreationError::PointNonExistent(format!(
                "{name} is attached to a point that does not exist"
            )));
        }
        // make sure points don't overlap
        for points in points.values().combinations(2) {
            let (p1, p2) = (points[0].pos(), points[1].pos());
            let threshold = 0.0001f64; // completely arbitrary, esp since we're unit-less

            if (p1.0 - p2.0).abs() <= threshold && (p1.1 - p2.1).abs() <= threshold {
                let n1 = names.get(&points[0].id()).unwrap();
                let n2 = names.get(&points[1].id()).unwrap();
                return Err(TrussCreationError::PointsOverlap(format!(
                    "Points {n1} and {n2} are within {threshold} of each other"
                )));
            }
        }

        Ok(Truss2D {
            points,
            connections,
            loads,
            supports,
            names,
        })
    }
    /// Produce a set of [TrussJoint2D]s that can be used to solve for the unknown forces in this
    /// truss.
    ///
    /// NOTE: currently there is not a way to convert the force components for a pin back into the
    /// human-friendly names, but everything else can be compared to this truss's names field.
    pub(crate) fn condense(&mut self) -> Vec<TrussJoint2D> {
        let mut joints: HashMap<SolverID, TrussJoint2D> = self
            .points
            .keys()
            .map(|p| (*p, TrussJoint2D::empty(*p)))
            .collect();
        for (id_1, id_2) in &self.connections {
            let new_id = id_1.concatenate(*id_2);
            let (p1, p2) = (
                self.points.get(id_1).unwrap(),
                self.points.get(id_2).unwrap(),
            );
            let (d1, d2) = (p1.direction_to(p2), p2.direction_to(p1));

            joints.get_mut(id_1).unwrap().add(Force2D::new(
                new_id,
                p1.clone(),
                d1,
                VectorComponent::Unknown,
            ));
            joints.get_mut(id_2).unwrap().add(Force2D::new(
                new_id,
                p2.clone(),
                d2,
                VectorComponent::Unknown,
            ));
        }
        for load in self.loads.values() {
            let joint = joints.get_mut(&load.at).unwrap();
            let point = self.points.get(&load.at).unwrap();
            joint.add(Force2D::new(load.id, point.clone(), load.dir, load.mag));
        }
        for (id, support) in &self.supports {
            match support {
                Support::Pin { at } => {
                    let x_id = id.concatenate(SolverID::new("x"));
                    let y_id = id.concatenate(SolverID::new("y"));

                    // TODO: this solution could leak memory in the names HashMap if this pin gets
                    //  deleted
                    let pin_name = self.names.get(id).unwrap();
                    let x_name = pin_name.clone() + " (x component)";
                    let y_name = pin_name.clone() + " (y component)";
                    self.names.insert(x_id, x_name);
                    self.names.insert(y_id, y_name);

                    let joint = joints.get_mut(at).unwrap();
                    let point = self.points.get(at).unwrap();

                    joint.add(Force2D::new(
                        x_id,
                        point.clone(),
                        Direction2D::right(),
                        VectorComponent::Unknown,
                    ));
                    joint.add(Force2D::new(
                        y_id,
                        point.clone(),
                        Direction2D::up(),
                        VectorComponent::Unknown,
                    ));
                }
                Support::Roller { at, dir } => {
                    let joint = joints.get_mut(at).unwrap();
                    let point = self.points.get(at).unwrap();
                    joint.add(Force2D::new(
                        *id,
                        point.clone(),
                        *dir,
                        VectorComponent::Unknown,
                    ));
                }
            }
        }

        joints.into_values().collect()
    }
}

/// Attempts to turn a toml::Value into a toml::Value::Array,
/// returns a Result<Array, ConversionError>
macro_rules! open_array {
    ($value: expr, $spot: ident) => {
        match $value {
            Value::Array(a) => Ok(a),
            _a => {
                let message = format!("Items must be declared as a table, not {_a:?}; at: ")
                    + stringify!($spot);
                Err(ConversionError::NotATable(message))
            }
        }
    };
    ($value: expr) => {
        match $value {
            Value::Array(a) => Ok(a),
            _a => {
                let message = format!("Items must be declared as a table, not {_a:?}");
                Err(ConversionError::NotATable(message))
            }
        }
    };
}
macro_rules! open_name {
    ($value: expr, $place: ident) => {
        match $value {
            Value::String(s) => Ok((s.as_str(), SolverID::new(s.as_str()))),
            _a => {
                let message =
                    format!("Expected string/name, but saw {_a:?}; at: ") + stringify!($place);
                Err(ConversionError::InvalidFormat(message))
            }
        }
    };
    ($value: expr) => {
        match $value {
            Value::String(s) => Ok((s.as_str(), SolverID::new(s.as_str()))),
            _a => {
                let message = format!("Expected string/name, but saw {_a:?}");
                Err(ConversionError::InvalidFormat(message))
            }
        }
    };
}

#[derive(Clone, Debug)]
pub(crate) enum ConversionError {
    NotATable(String),
    IncorrectLength(String),
    InvalidFormat(String),
    ConflictingDefinitions(String),
}
impl std::fmt::Display for ConversionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use ConversionError as CE;
        match self {
            CE::NotATable(_) => write!(f, "Did not get a toml table where expected: ")?,
            CE::IncorrectLength(_) => write!(f, "Declared item has the wrong length:")?,
            CE::InvalidFormat(_) => {
                write!(f, "An item was declared with the wrong values or format:")?
            }
            CE::ConflictingDefinitions(_) => write!(
                f,
                "Conflicting definitions were found for an item; you can use \'#\' to ignore one"
            )?,
        }
        let message = match self {
            CE::NotATable(s) => s.as_str(),
            CE::IncorrectLength(s) => s.as_str(),
            CE::InvalidFormat(s) => s.as_str(),
            CE::ConflictingDefinitions(s) => s.as_str(),
        };
        write!(f, "{message}")
    }
}

/// Result from attempted to parse a toml table into points.
///
/// Result<HashMap<SolverID, Point2D>, ConversionError>
type PointsResult = Result<HashMap<SolverID, Point2D>, ConversionError>;
/// Attempt to convert the given [Value] into [Point2D]s, may fail with a
/// [ConversionError] that contains more details. Any returned parsing errors are likely not
/// recoverable, as they require modification to the input file.
fn into_points(t: &Value, names: &mut HashMap<SolverID, String>) -> PointsResult {
    let table = open_array!(t, points_array)?;
    let mut points = HashMap::with_capacity(table.len());

    for raw_point in table {
        let raw_point = open_array!(raw_point, point_specific)?;
        match raw_point.len() {
            0 => continue,
            2 | 4 => {}, // Handle later
            _ => return Err(
                ConversionError::IncorrectLength(format!(
                    "Point {:?} declared with the wrong number of items! Must be 2 (for the origin) or 4 (otherwise)",
                    raw_point.first().unwrap() // allowed bc zero length items are ignored
                ))
            ),
        };
        let (name, id) = open_name!(&raw_point[0], point_name_inner)?;
        if names.insert(id, name.to_string()).is_some() {
            return Err(ConversionError::ConflictingDefinitions(format!(
                "point {name} has been declared twice!"
            )));
        };
        if raw_point.len() == 2 {
            if Some("Origin") != raw_point[1].as_str() {
                let m = format!(
                    "point {name} has 2 items, but is declared with {:?} instead of Origin",
                    &raw_point[1]
                );
                return Err(ConversionError::InvalidFormat(m));
            }
            points.insert(id, Point2D::origin(id));
            continue;
        }

        // the indexing here won't panic bc we've checked the len must be [0, 2, 4] & we've already
        // handled the 0 and 2 cases (so it must be 4)
        let (v1, v2) = match (&raw_point[2], &raw_point[3]) {
            (Value::Integer(i1), Value::Integer(i2)) => (*i1 as f64, *i2 as f64),
            (Value::Float(f1), Value::Float(f2)) => (*f1, *f2),
            (Value::Integer(i1), Value::Float(f1)) => (*i1 as f64, *f1),
            (Value::Float(f1), Value::Integer(i1)) => (*f1, *i1 as f64),
            _ => {
                return Err(ConversionError::InvalidFormat(format!(
                    "Point {name} must have 2 numbers for position, saw {:?} and {:?}",
                    &raw_point[2], &raw_point[3]
                )))
            }
        };
        let p = match &raw_point[1] {
            Value::String(s) if s.as_str() == "Cartesian" => Point2D::cartesian(id, v1, v2),
            Value::String(s) if s.as_str() == "Polar" => Point2D::polar(id, v1, v2),
            _ => {
                return Err(ConversionError::InvalidFormat(format!(
                    "Point {name} must be Cartesian, Polar, or Origin"
                )))
            }
        };
        points.insert(id, p);
    }
    Ok(points)
}

type ConnectionsResult = Result<Vec<(SolverID, SolverID)>, ConversionError>;
fn into_connections(t: &Value, names: &mut HashMap<SolverID, String>) -> ConnectionsResult {
    let raw_connections = open_array!(t, truss_connections_table)?;
    let mut connections = Vec::with_capacity(raw_connections.len());

    for raw_connection in raw_connections {
        let raw = open_array!(raw_connection, truss_connections_inner)?;
        match raw.len() {
            0 => continue,
            2 => {}
            _ => {
                return Err(ConversionError::IncorrectLength(format!(
                    "Internal member declared with incorrect length, should be 2, but saw {raw:?}"
                )))
            }
        };

        let (name1, id1) = open_name!(&raw[0])?;
        let (name2, id2) = open_name!(&raw[1])?;
        // sorting is technically not completely necessary bc concatenating IDs is the same for the
        // same two IDs, but it's better to be consistent ig
        match name1.cmp(name2) {
            Ordering::Greater => connections.push((id1, id2)),
            Ordering::Equal => {
                #[cfg(debug_assertions)]
                eprintln!("Warning: 0 length member {name1} declared!");
                continue;
            }
            Ordering::Less => connections.push((id2, id1)),
        }
        let member_id = id1.concatenate(id2);
        let member_name = match name1.cmp(name2) {
            Ordering::Greater => format!("Member {name1}<->{name2}"),
            Ordering::Less => format!("Member {name2}<->{name1}"),
            Ordering::Equal => unreachable!(),
        };
        if names.insert(member_id, member_name).is_some() {
            return Err(ConversionError::ConflictingDefinitions(format!(
                "Member with points {name1} & {name2} has been declared more than once!"
            )));
        }
    }
    Ok(connections)
}

type LoadsResult = Result<HashMap<SolverID, AppliedLoad>, ConversionError>;
fn into_loads(t: &Value, names: &mut HashMap<SolverID, String>) -> LoadsResult {
    let raw_loads = open_array!(t, applied_loads_outer)?;
    let mut loads = HashMap::with_capacity(raw_loads.len());
    let mut counter = 1;

    for raw_load in raw_loads {
        let raw_load = open_array!(raw_load, applied_loads_inner)?;
        match raw_load.len() {
            0 => continue,
            3 | 4 => {}
            _ => {
                return Err(ConversionError::IncorrectLength(format!(
                    "applied loads must have 3 or 4 items defined, {:?} does not",
                    raw_load[0]
                )))
            }
        }
        let (p_name, _) = open_name!(&raw_load[0], get_name_applied_load)?;
        let l_name = format!("Load {counter} at {p_name}");
        let l_id = SolverID::new(&l_name);
        counter += 1;

        // indexing here is safe bc guaranteed that len is 3 or 4
        let magnitude = match &raw_load[1] {
            Value::Float(f) => VectorComponent::KnownExactly(*f),
            Value::Integer(i) => VectorComponent::KnownExactly(*i as f64),
            _a => {
                return Err(ConversionError::InvalidFormat(format!(
                    "applied loads must have a known magnitude, load {l_name} has {_a:?}"
                )))
            }
        };

        const ACCEPTABLE: [&str; 5] = ["Up", "Down", "Left", "Right", "Polar"];
        if raw_load[2].as_str().is_none() || !ACCEPTABLE.contains(&raw_load[2].as_str().unwrap()) {
            return Err(ConversionError::InvalidFormat(format!(
                "applied loads must have a direction of {ACCEPTABLE:?}, but {l_name} has {:?}",
                raw_load[2]
            )));
        }
        let dir = match raw_load[2].as_str().unwrap() {
            "Up" => Direction2D::up(),
            "Down" => Direction2D::down(),
            "Left" => Direction2D::left(),
            "Right" => Direction2D::right(),
            "Polar" => {
                let e = ConversionError::InvalidFormat(format!(
                    "polar directions must specify an angle, saw {:?} for {l_name}",
                    raw_load.get(3)
                ));
                let v = raw_load.get(3).ok_or(e.clone())?;
                match v {
                    Value::Integer(i) => Direction2D::from_degrees(*i as f64),
                    Value::Float(f) => Direction2D::from_degrees(*f),
                    _ => return Err(e),
                }
            }
            _ => unreachable!(),
        };
        // no duplicates, bc a point can have multiple loads & the names are created to be unique
        names.insert(l_id, l_name);
        loads.insert(
            l_id,
            AppliedLoad {
                id: l_id,
                at: SolverID::new(p_name),
                dir,
                mag: magnitude,
            },
        );
    }

    Ok(loads)
}

type SupportsResult = Result<HashMap<SolverID, Support>, ConversionError>;
fn into_supports(t: &Value, names: &mut HashMap<SolverID, String>) -> SupportsResult {
    let raw_supports = open_array!(t, supports_table_outer)?;
    let mut supports = HashMap::with_capacity(raw_supports.len());

    let mut roller_count = 1;
    let mut pin_count = 1;

    for raw_support in raw_supports {
        let raw_support = open_array!(raw_support, supports_inner)?;
        match raw_support.len() {
            0 => continue,
            2 | 3 => {}
            _ => {
                return Err(ConversionError::IncorrectLength(format!(
                    "Support at {} should have length 2 or 3",
                    &raw_support[0]
                )))
            }
        }
        let (p_name, p_id) = open_name!(&raw_support[0], support_item_inner)?;
        match raw_support[1].as_str() {
            Some("Pin") => {
                let s_name = format!("Pin {pin_count} at {p_name}");
                supports.insert(SolverID::new(&s_name), Support::Pin { at: p_id });
                names.insert(SolverID::new(&s_name), s_name); // multiple supports @ same point are allowed
                pin_count += 1;
            }
            Some("Roller") => {
                let s_name = format!("Roller {roller_count} at {p_name}");
                let dir = match raw_support.get(2) {
                    Some(Value::String(s)) if s.as_str() == "Up" => Direction2D::up(),
                    Some(Value::String(s)) if s.as_str() == "Down" => Direction2D::down(),
                    Some(Value::String(s)) if s.as_str() == "Left" => Direction2D::left(),
                    Some(Value::String(s)) if s.as_str() == "Right" => Direction2D::right(),
                    _a => {
                        return Err(ConversionError::InvalidFormat(format!(
                            "Roller direction must be Up, Down, Left, Right; {s_name} has {_a:?}"
                        )))
                    }
                };
                supports.insert(SolverID::new(&s_name), Support::Roller { at: p_id, dir });
                names.insert(SolverID::new(&s_name), s_name);
                roller_count += 1;
            }
            _a => {
                return Err(ConversionError::InvalidFormat(format!(
                    "Support at {p_name} must be either Pin or Roller, not {_a:?}"
                )))
            }
        };
    }

    Ok(supports)
}
