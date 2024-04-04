use std::cmp::Ordering;
use std::{collections::BTreeMap, slice::Iter};

use toml::{Table, Value};

use crate::solver;


/// Turn a iterator over toml Values into a pair of numbers. This will warn if there are more than 2
/// values in the iterator and panic if there are less than 2 (or if one/both isn't a number). This
/// function works with both toml::Value::Float and toml::Value::Integer
fn parse_coordinate_pair(mut value: Iter<Value>, identifier: &str) -> (f64, f64) {
    let (a, b) = (value.next(), value.next());
    if let (Some(Value::Float(a)), Some(Value::Float(b))) = (a, b) {
        if value.next().is_some() {
            eprintln!("Warning! extra items in point definition!");
            eprintln!(
                "Point {identifier}: {} has {} extra arguments (they were ignored).",
                solver::SolverID::new(identifier),
                value.len() + 1
            );
        }

        return (*a, *b);
    } else if let (Some(Value::Integer(a)), Some(Value::Integer(b))) = (a, b) {
        if value.next().is_some() {
            eprintln!("Warning! extra items in point definition!");
            eprintln!(
                "Point {identifier}: {} has {} extra arguments (they were ignored).",
                solver::SolverID::new(identifier),
                value.len() + 1
            );
        }

        return (*a as f64, *b as f64);
    }
    eprintln!("Error: expected two numbers in the format \'a, b\' in the declaration of a point");
    eprintln!(
        "Saw \'{:?}, {:?}\' in the description of point {identifier}: {}.",
        a,
        b,
        solver::SolverID::new(identifier)
    );
    panic!("Couldn't parse point coordinates!");
}

/// Converts a toml value into an array of toml values or panics
macro_rules! array_me {
    ($value: expr) => {
        if let toml::Value::Array(a) = $value {
            a
        } else {
            todo!()
        }
    };
}

// TODO: better error messages by 1) impl Error 2) including name info to find these
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PointValidationError {}
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum EquilibriumError {}

#[allow(unused)]
fn validate_points(points: &[solver::Point2D]) -> Result<(), PointValidationError> {
    todo!()
}
#[allow(unused)]
fn validate_static_equilibrium() -> Result<(), EquilibriumError> {
    todo!()
}
/// Takes in a toml::Value::Array and parses it into an array of Point2D for the
/// solver to use. The function signature doesn't capture this, but the Value passed in
/// must be the Value::Array(_) variant, or the function will panic (TODO: change to erroring)
pub(crate) fn parse_points_from_array(
    array: &Value,
) -> BTreeMap<solver::SolverID, solver::Point2D> {
    let raw_table = array_me!(array);
    let mut points: BTreeMap<solver::SolverID, solver::Point2D> = BTreeMap::new();

    for entry in raw_table {
        // maybe a tokenize! macro would be useful
        let mut tokens = match entry {
            Value::Array(a) if a.len() >= 2 => {a.iter()},
            Value::Array(a) => panic!("Point \'{:?}\' declared with too few values!", a[0]),
            other => panic!("Error: expecting array of values to parse points from! Saw {:?}", other),
        };
        
        let (id, point_name) = match tokens.next() {
            Some(Value::String(s)) => (solver::SolverID::new(s.as_str()), s.as_str()),
            other => panic!("Expected a point name (text), but got \'{:?}\'", other)
        };
            
        let point = match tokens.next() {
            Some(Value::String(s)) if s.as_str() == "Origin" => solver::Point2D::origin(id),
            Some(Value::String(s)) if s.as_str() == "Cartesian" => {
                let (x, y) = parse_coordinate_pair(tokens, point_name);
                solver::Point2D::cartesian(id, x, y)
            }
            Some(Value::String(s)) if s.as_str() == "Polar" => {
                let (r, theta) = parse_coordinate_pair(tokens, point_name);
                solver::Point2D::polar(id, r, theta)
            }
            Some(other) => panic!("Invalid Point Definition! Expected one of [Origin, Cartesian, Polar] but saw \'{:?}\' at \'{point_name}\'", other),
            None => panic!("Point missing definition type: point \'{point_name}\' has no location"),
        };
        points.insert(id, point);
    }
    points
}
pub(crate) fn parse_loads_from_array(
    array: &Value,
    points: &BTreeMap<solver::SolverID, solver::Point2D>,
) -> Vec<solver::Force2D> {
    let raw_forces = array_me!(array);
    let mut forces: Vec<solver::Force2D> = Vec::new();

    for raw_force in raw_forces {
        let mut tokens = array_me!(raw_force).iter();
        
        let (point_id, name) = match tokens.next() {
            Some(Value::String(s)) => (solver::SolverID::new(s.as_str()), s.as_str()),
            other => panic!("Expected the name of a point (text), but got \'{:?}\'", other)
        };

        let magnitude = match tokens.next() {
            Some(Value::Float(f)) => *f,
            Some(Value::Integer(i)) => *i as f64,
            other => panic!("Load \'{name}\' has improperly defined magnitude, expended a number but saw \'{:?}\'", other),
        };

        let direction = match tokens.next() {
            Some(Value::String(s)) if s.as_str() == "Up" => solver::Direction2D::from_angle(90.0),
            Some(Value::String(s)) if s.as_str() == "Down" => {
                solver::Direction2D::from_angle(270.0)
            }
            Some(Value::String(s)) if s.as_str() == "Left" => {
                solver::Direction2D::from_angle(180.0)
            }
            Some(Value::String(s)) if s.as_str() == "Right" => solver::Direction2D::from_angle(0.0),
            Some(Value::String(s)) if s.as_str() == "Polar" => {
                let t = tokens.next();
                if let Some(Value::Float(f)) = t {
                    solver::Direction2D::from_angle(*f)
                } else if let Some(Value::Integer(i)) = t {
                    solver::Direction2D::from_angle(*i as f64)
                } else {
                    panic!("Invalid angle provided for load \'{name}\', expected a number and saw \'{:?}\'", t);
                }
            }
            Some(Value::String(s)) => panic!("Load \'{name}\' has in invalid direction: \'{s}\' must be [Up, Down, Left, Right, Polar]"),
            Some(other) => panic!("Invalid value in the direction for force \'{name}\', saw {:?}", other),
            None => panic!("Saw an applied load with no direction! Load \'{name}\' must have a direction"),
        };

        // TODO: what to do about parsing these names for loads
        let force = solver::Force2D::new(
            solver::SolverID::new("some load applied"),
            match points.get(&point_id){
                Some(p) => p.clone(),
                None => panic!("Loads must be attached to a valid point, \'{name}\' does not exist!"),
            },
            direction,
            solver::VectorComponent::KnownExactly(magnitude),
        );
        forces.push(force);
    }
    forces
}

pub(crate) fn construct_member_pairs(array: &Value) -> Vec<(solver::SolverID, solver::SolverID)> {
    let raw_members = array_me!(array);
    let mut members: Vec<(solver::SolverID, solver::SolverID)> = Vec::new();

    for raw_member in raw_members {
        let mut tokens = array_me!(raw_member).iter();

        // To ensure that the member AB should have the same order at member BA, so we sort
        let pair = match (tokens.next(), tokens.next()) {
            (Some(Value::String(s1)), Some(Value::String(s2))) => match s1.cmp(s2) {
                Ordering::Greater => (solver::SolverID::new(s1.as_str()), solver::SolverID::new(s2.as_str())),
                Ordering::Less => (solver::SolverID::new(s2.as_str()), solver::SolverID::new(s1.as_str())),
                // this assumes the Ordering::Equal strings are actually the same string
                Ordering::Equal => {eprintln!("Non-existent member declared! {:?}, {:?}", s1, s2); continue},
            },
            (a, b) => panic!("Incorrect structural member definition! Expected \'point1 name, point2 name\', got \'{:?}, {:?}\'", a, b)
        };
        members.push(pair);
    }

    // this should be fine? bc the elements to remove should be identical so doesn't matter how
    // they're sorted ??
    members.sort();
    members.dedup();
    members
}
pub(crate) fn generate_support_reactions(array: &Value) -> Vec<solver::Force2D> {
    let _raw_supports = array_me!(array);
    todo!()
}
#[allow(unused)]
pub(crate) fn parse_problem(file: String) -> Vec<solver::TrussJoint2D> {
    let data = file.parse::<Table>().unwrap();
    let points = parse_points_from_array(data.get("points").unwrap());
    let loads = parse_loads_from_array(data.get("loads").unwrap(), &points);

    let members = construct_member_pairs(data.get("members").unwrap());
    let mut internal_forces: Vec<solver::Force2D> = Vec::new();
    for (id1, id2) in members {
        let new_id = id1.concatenate(id2);
        let (p1, p2) = (points.get(&id1).unwrap(), points.get(&id2).unwrap());
        let (d1, d2) = (p1.direction_to(p2), p2.direction_to(p1));
        internal_forces.push(solver::Force2D::new(
            new_id,
            p1.clone(),
            d1,
            solver::VectorComponent::Unknown,
        ));
        internal_forces.push(solver::Force2D::new(
            new_id,
            p2.clone(),
            d2,
            solver::VectorComponent::Unknown,
        ));
    }

    let support_reactions = generate_support_reactions(data.get("supports").unwrap());

    let mut joints = {
        let mut map: BTreeMap<solver::SolverID, solver::TrussJoint2D> = BTreeMap::new();
        for (id, _) in points.iter() {
            map.insert(*id, solver::TrussJoint2D::empty(*id));
        }
        map
    };

    for force in loads {
        let joint = match joints.get_mut(&force.point_id()) {
            Some(j) => j,
            None => todo!()
        };

        joint.add(force);
    }
    for force in internal_forces {
        let joint = match joints.get_mut(&force.point_id()) {
            Some(j) => j,
            None => todo!()
        };

        joint.add(force);
    }
    for force in support_reactions {
        let joint = match joints.get_mut(&force.point_id()) {
            Some(j) => j,
            None => todo!()
        };

        joint.add(force);
    }
    joints.into_iter().map(|(id, joint)| joint).collect()
}
