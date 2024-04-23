use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::{collections::BTreeMap, slice::Iter};

use toml::{Table, Value};

use crate::solver;
use crate::solver::{Direction2D, Force2D, Point2D, SolverID, TrussJoint2D};

pub(crate) struct ProblemInformation {
    pub(crate) name: String,
    pub(crate) debug_info: bool,
    pub(crate) file_write: bool,
}
pub(crate) fn get_problem_information(problem: &str) -> ProblemInformation {
    let table = problem.parse::<Table>().unwrap();
    let name = match table.get("name") {
        Some(Value::String(s)) => s.clone(),
        Some(_) => String::from("(Invalid name--not text)"),
        None => String::from("No name"),
    };
    let debug_info = match table.get("debug") {
        Some(Value::Boolean(b)) => *b,
        _ => false,
    };
    let file_write = match table.get("write-file") {
        Some(Value::Boolean(b)) => *b,
        _ => false,
    };

    ProblemInformation {
        name,
        debug_info,
        file_write,
    }
}

/// Turn an iterator over toml Values into a pair of numbers. This will warn if there are more than 2
/// values in the iterator and panic if there are less than 2 (or if one/both isn't a number). This
/// function works with both toml::Value::Float and toml::Value::Integer
fn parse_coordinate_pair(mut value: Iter<Value>, identifier: &str) -> (f64, f64) {
    let (a, b) = (value.next(), value.next());
    if value.next().is_some() {
        eprintln!("Warning! extra items in point definition!");
        eprintln!(
            "Point {identifier}: {} has {} extra arguments (they were ignored).",
            SolverID::new(identifier),
            value.len() + 1
        );
    }
    match (a, b) {
        (Some(Value::Float(a)), Some(Value::Float(b))) => return (*a, *b),
        (Some(Value::Integer(a)), Some(Value::Integer(b))) => return (*a as f64, *b as f64),
        (Some(Value::Float(a)), Some(Value::Integer(b))) => return (*a, *b as f64),
        (Some(Value::Integer(a)), Some(Value::Float(b))) => return (*a as f64, *b),
        _ => {}
    }

    eprintln!("Error: expected two numbers in the format \'a, b\' in the declaration of a point");
    eprintln!(
        "Saw \'{:?}, {:?}\' in the description of point {identifier}: {}.",
        a,
        b,
        SolverID::new(identifier)
    );
    panic!("Couldn't parse point coordinates!");
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

// TODO: better error messages by 1) impl Error 2) including name info to find these
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PointValidationError {
    DuplicatePosition,
}
#[warn(incomplete_features)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum EquilibriumError {}

// TODO: this function should actually do anything
#[allow(unused)]
pub(crate) fn validate_points(
    points: &BTreeMap<SolverID, Point2D>,
) -> Result<(), PointValidationError> {
    let mut names: BTreeSet<SolverID> = BTreeSet::new();
    let mut positions: Vec<(f64, f64)> = Vec::new();

    const TOLERANCE: f64 = 0.00001f64;
    let to_remove: Vec<usize> = Vec::new();
    for point in points.values() {
        let (x, y) = point.coords();
        if positions.iter().any(|(a, b)| {
            a - TOLERANCE <= x && x <= a + TOLERANCE && b - TOLERANCE <= y && y <= b + TOLERANCE
        }) {
            return Err(PointValidationError::DuplicatePosition);
        }
    }
    Ok(())
}

#[warn(incomplete_features)]
#[allow(unused)]
fn validate_static_equilibrium() -> Result<(), EquilibriumError> {
    todo!()
}

/// Takes in a toml::Value::Array and parses it into an array of Point2D for the
/// solver to use. The function signature doesn't capture this, but the Value passed in
/// must be the Value::Array(_) variant, or the function will panic
pub(crate) fn parse_points(
    toml_array: &Value,
    name_map: &mut BTreeMap<SolverID, String>,
    print_debug: bool,
) -> BTreeMap<SolverID, Point2D> {
    let raw_table = array_me!(toml_array);
    let mut points: BTreeMap<SolverID, Point2D> = BTreeMap::new();

    for entry in raw_table {
        // maybe a tokenize! macro would be useful
        let mut tokens = match entry {
            Value::Array(a) if a.len() >= 2 => a.iter(),
            Value::Array(a) => panic!("Point \'{:?}\' declared with too few values!", a[0]),
            other => panic!(
                "Error: expecting array of values to parse points from! Saw {:?}",
                other
            ),
        };

        let (id, point_name) = match tokens.next() {
            Some(Value::String(s)) => (SolverID::new(s.as_str()), s.as_str()),
            other => panic!("Expected a point name (text), but got \'{:?}\'", other),
        };

        let point = match tokens.next() {
            Some(Value::String(s)) if s.as_str() == "Origin" => Point2D::origin(id),
            Some(Value::String(s)) if s.as_str() == "Cartesian" => {
                let (x, y) = parse_coordinate_pair(tokens, point_name);
                Point2D::cartesian(id, x, y)
            }
            Some(Value::String(s)) if s.as_str() == "Polar" => {
                let (r, theta) = parse_coordinate_pair(tokens, point_name);
                Point2D::polar(id, r, theta)
            }
            Some(other) => panic!("Invalid Point Definition! Expected one of [Origin, Cartesian, Polar] but saw \'{:?}\' at \'{point_name}\'", other),
            None => panic!("Point missing definition type: point \'{point_name}\' has no location"),
        };

        if print_debug {
            println!("Parsed point {point_name} to {point}");
        }

        match points.insert(id, point) {
            None => {} // Point was not already created (good ending)
            Some(_) => {
                eprintln!("Warning! Duplicate point name declared: \'\'!");
                panic!("Comment out the extra point with \'#\' to save it for later");
            }
        };
        name_map.insert(id, point_name.to_string());
    }
    points
}
pub(crate) fn parse_loads(
    toml_array: &Value,
    points: &BTreeMap<SolverID, Point2D>,
    name_map: &mut BTreeMap<SolverID, String>,
) -> Vec<Force2D> {
    let raw_forces = array_me!(toml_array);
    let mut forces: Vec<Force2D> = Vec::new();

    for raw_force in raw_forces {
        let mut tokens = array_me!(raw_force).iter();

        #[allow(unused)] // Used in format strings
        let (point_id, point_name) = match tokens.next() {
            Some(Value::String(s)) => (SolverID::new(s.as_str()), s.as_str()),
            other => panic!(
                "Expected the name of a point (text), but got \'{:?}\'",
                other
            ),
        };

        let magnitude = match tokens.next() {
            Some(Value::Float(f)) => *f,
            Some(Value::Integer(i)) => *i as f64,
            other => panic!("Load \'{point_name}\' has improperly defined magnitude, expended a number but saw \'{:?}\'", other),
        };

        let direction = match tokens.next() {
            Some(Value::String(s)) if s.as_str() == "Up" => Direction2D::from_angle(90.0),
            Some(Value::String(s)) if s.as_str() == "Down" => {
                Direction2D::from_angle(270.0)
            }
            Some(Value::String(s)) if s.as_str() == "Left" => {
                Direction2D::from_angle(180.0)
            }
            Some(Value::String(s)) if s.as_str() == "Right" => Direction2D::from_angle(0.0),
            Some(Value::String(s)) if s.as_str() == "Polar" => match tokens.next() {
                Some(Value::Float(f)) => Direction2D::from_angle(*f),
                Some(Value::Integer(i)) => Direction2D::from_angle(*i as f64),
                Some(o) => panic!("Invalid angle provided for load \'{point_name}\', expected a number and saw \'{:?}\'", o),
                None => panic!("No direction provided for load \'{point_name}\'!"),
            }
            Some(Value::String(_s)) => panic!("Load \'{point_name}\' has in invalid direction: \'{_s}\' must be [Up, Down, Left, Right, Polar]"),
            Some(other) => panic!("Invalid value in the direction for force \'{point_name}\', saw {:?}", other),
            None => panic!("Saw an applied load with no direction! Load \'{point_name}\' must have a direction"),
        };

        // Note: these names are randomized, but that means we can't get an actual name back
        let load_name_unique = format!("Load {point_name}{}", rand::random::<usize>());
        let force = Force2D::new(
            SolverID::new(&load_name_unique),
            match points.get(&point_id) {
                Some(p) => p.clone(),
                None => {
                    panic!(
                        "Loads must be attached to a valid point, \'{point_name}\' does not exist!"
                    )
                }
            },
            direction,
            solver::VectorComponent::KnownExactly(magnitude),
        );

        name_map.insert(SolverID::new(&load_name_unique), load_name_unique);
        forces.push(force);
    }
    forces
}

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
                    Direction2D::from_angle(0f64),
                    solver::VectorComponent::Unknown,
                ));
                support_reactions.push(Force2D::new(
                    idy,
                    attached_point.clone(),
                    Direction2D::from_angle(90f64),
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
                        Direction::Up => Direction2D::from_angle(90f64),
                        Direction::Down => Direction2D::from_angle(270f64),
                        Direction::Left => Direction2D::from_angle(0f64),
                        Direction::Right => Direction2D::from_angle(180f64),
                    },
                    solver::VectorComponent::KnownPositive,
                ));
            }
        };
    }
    support_reactions
}

fn generate_internal_forces(
    array: &Value,
    name_map: &mut BTreeMap<SolverID, String>,
    points: &BTreeMap<SolverID, Point2D>,
) -> Vec<Force2D> {
    let mut internal_forces: Vec<Force2D> = Vec::new();
    let raw_members = array_me!(array);
    let mut members: Vec<(SolverID, SolverID)> = Vec::new();

    for raw_member in raw_members {
        let mut tokens = array_me!(raw_member).iter();

        // To ensure that the member AB should have the same order at member BA, so we sort
        // the name map part is bad, but it will prob work for now
        let (name1, name2) = match (tokens.next(), tokens.next()) {
            (Some(Value::String(s1)), Some(Value::String(s2))) => match s1.cmp(s2) {
                Ordering::Greater => (s1, s2),
                Ordering::Less => (s2, s1),
                Ordering::Equal => {eprintln!("Non-existent member declared! {:?}, {:?}", s1, s2); continue}
            }
            (a, b) => panic!("Incorrect structural member definition! Expected \'point1 name, point2 name\', got \'{:?}, {:?}\'", a, b)
        };
        let (id1, id2) = (SolverID::new(name1), SolverID::new(name2));
        // ideally this would only be done w/ the de-duped list, but I can't currently think of how
        name_map.insert(id1.concatenate(id2), format!("{}<->{}", name1, name2));
        members.push((id1, id2));
    }

    // this should be fine? bc the elements to remove should be identical so doesn't matter how
    // they're sorted ??
    members.sort();
    members.dedup();

    for (id1, id2) in &members {
        let new_id = id1.concatenate(*id2);
        //let (p1, p2) = (points.get(id1).unwrap(), points.get(id2).unwrap());
        let (p1, p2) = match (points.get(id1), points.get(id2)) {
            (Some(a), Some(b)) => (a, b),
            (Some(_), None) => panic!("Member declared from point that does not exist: {}", id2),
            (None, Some(_)) => panic!("Member declared from point that does not exist: {}", id1),
            _ => unreachable!(
                "Unexpected result in matching point search results when building member forces"
            ),
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
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ParsingError {
    InvalidTOMLFile,
    IncorrectPoints(PointValidationError),
    #[allow(unused)]
    NotInEquilibrium,
}
impl Display for ParsingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidTOMLFile => write!(f, "invalid TOML file"),
            Self::IncorrectPoints(e) => write!(f, "incorrectly defined points--{:#?}", e),
            Self::NotInEquilibrium => write!(f, "the initial problem is not in equilibrium"),
        }
    }
}
impl From<PointValidationError> for ParsingError {
    fn from(value: PointValidationError) -> Self {
        ParsingError::IncorrectPoints(value)
    }
}
impl Error for ParsingError {}

pub struct ParsedProblem {
    pub(crate) name_map: BTreeMap<SolverID, String>,
    pub(crate) joints: Vec<TrussJoint2D>,
}
#[allow(unused)]
pub(crate) fn parse_problem(file: String, debug_info: bool) -> Result<ParsedProblem, ParsingError> {
    let toml_file = match file.parse::<Table>() {
        Ok(a) => a,
        Err(e) => return Err(ParsingError::InvalidTOMLFile),
    };
    let mut names_record = BTreeMap::new();

    let points = parse_points(
        toml_file
            .get("points")
            .expect("TOML file must define a points array!"),
        &mut names_record,
        debug_info,
    );

    let internal_forces = generate_internal_forces(
        toml_file
            .get("members")
            .expect("TOML file must define the members composing the truss!"),
        &mut names_record,
        &points,
    );

    let loads = parse_loads(
        toml_file
            .get("loads")
            .expect("TOML file must define a loads array!"),
        &points,
        &mut names_record,
    );

    let support_reactions = generate_support_reactions(
        toml_file
            .get("supports")
            .expect("TOML file must define a \'supports\' array"),
        &points,
        &mut names_record,
    );

    // TODO: include the validations for points, forces, and whatever else

    let mut joints = {
        let mut map: BTreeMap<SolverID, TrussJoint2D> = BTreeMap::new();
        for (id, _) in points.iter() {
            map.insert(*id, TrussJoint2D::empty(*id));
        }
        map
    };

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
