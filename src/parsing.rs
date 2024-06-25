use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fmt::Formatter;
use std::io::Write;
use itertools::Itertools;

use nalgebra as na;
use toml::{Table, Value};

use crate::solver;
use crate::solver::{Direction2D, Force2D, Point2D, SolverID, TrussJoint2D};

pub(crate) struct ProblemInformation {
    pub(crate) name: String,
    pub(crate) debug: DebugInfo,
    pub(crate) file_write: bool,
}
/// Grabs important settings information from a TOML file. This function will not error.
pub(crate) fn get_problem_information(problem: &str) -> ProblemInformation {
    let table = problem.parse::<Table>().unwrap();
    let name = match table.get("name") {
        Some(Value::String(s)) => s.clone(),
        Some(_) => String::from("(Invalid name--not text)"),
        None => String::from("No name"),
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
            .unwrap();
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
    #[cfg(test)] // Used in tests & otherwise should always have a valid output
    pub(crate) fn empty() -> Self {
        DebugInfo {
            enabled: false,
            output: Box::new(EmptyWriter()),
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
                write!(self.output, "{:.14}, ", *v);
            }
            writeln!(self.output, "]");
        }
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
                eprintln!("Load acting at \'{}\' has the wrong number of values declared!", a[0]);
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
            solver::VectorComponent::KnownExactly(magnitude),
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
                    solver::VectorComponent::Unknown,
                ));
                support_reactions.push(Force2D::new(
                    idy,
                    attached_point.clone(),
                    Direction2D::from_degrees(90f64),
                    solver::VectorComponent::Unknown,
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
                    solver::VectorComponent::KnownPositive,
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
            solver::VectorComponent::Unknown,
        ));
        internal_forces.push(Force2D::new(
            new_id,
            p2.clone(),
            d2,
            solver::VectorComponent::Unknown,
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
        let raw = toml_file.get("points").ok_or(ParsingError::MissingPointsTable)?;
        let p = parse_points(raw, &mut names_record, debug);
        if let Err(e) = validate_points(&p) {
            eprintln!("WARNING: {e}");
        }
        p
    };

    let internal_forces = {
        let i = toml_file.get("members").ok_or(ParsingError::MissingMembersTable)?;
        generate_internal_forces(i, &points, &mut names_record)
    };
    let loads = {
        let l = toml_file.get("loads").ok_or(ParsingError::MissingLoadsTable)?;
        parse_loads(l, &points, &mut names_record)
    };
    let support_reactions = {
        let s = toml_file.get("supports").ok_or(ParsingError::MissingSupportsTable)?;
        generate_support_reactions(s, &points, &mut names_record)
    };

    // TODO: include the validations for points, forces, and whatever else

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
    
    // TODO: There are probably other things that need to be checked 
    
    Ok(())
}