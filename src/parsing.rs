/* Order / flow for parsing to/from documents:
This program will solve a general truss given in a specific file format. It is restricted to
trusses with their loads and supports in the same plane as the truss itself.

1) The problem starts as a file with the following defined (either absolute or relative):
  > All relevant points (either absolute or relative)
  > All applied loads w/ known magnitude attached to a point
  > All external supports
  > One point specified to be the base point
  > Point pairs that are connected with truss members
  > Internal connections are assumed to be pins (TODO: get this checked)
2) The problem is parsed into a list of points w/ known positions & unique IDs / names
  > Each relevant point in the problem (any external support, or joint in the truss) has
    a known absolute position (in the coordinate system) and a unique ID
  > There are no duplicate points (e.g. if two things are at the same point, they have the same point ID)
3) A list of all applicable loads for each point is created
  > Contains support reactions, internal forces, and external loads
  >

3) A list of all loads, support reactions, and internal forces is created for each point
  > Each force has a unique ID and is associated to a specific point

TODO: finished writing this:
4) The list of loads (etc.) is converted into lists of each force and moment
  > The force / moment vectors all have a known origin point
  > Each vector is represented component wise and each component is either known or unknown
  > There is no difference between the reactions and applied loads (although is is possible
    to see where each vector came from)
5) The lists are split into separate pieces where the equations of equilibrium can be applied
  > Forces are split up non-uniquely (e.g. the same force can be in more than one piece)
  > Each piece represents one body
6) Each body is solved via conversion to a matrix equation


7) The results are pulled from the individual rigid bodies and compared / checked
8) The solved values are put back into the lists & an output file is generated
  > The generated output file has all the resolved information (e.g. point positions & vector components)
  > The names / order is the same as the input file
*/
use std::slice::Iter;

use toml::{Table, Value};

use crate::solver;

fn value_to_string(value: &Value) -> &str {
    if let Value::String(s) = value {
        return s;
    }
    eprintln!("Can not convert non-string value to string!");
    eprintln!(
        "Expected to find a toml::Value::String, but saw \'{:?}\' instead",
        value
    );
    panic!("Invalid type, see above error");
}

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

// TODO: better error messages by 1) impl Error 2) including name info to find these
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PointValidationError {
    MultipleOrigins,
    Overlapping,
    IdenticalNames,
}
#[allow(unused)]
fn validate_points() {
    todo!()
}
/// Takes in a toml::Value::Array and parses it into an array of Point2D for the
/// solver to use. The function signature doesn't capture this, but the Value passed in
/// must be the Value::Array(_) variant, or the function will panic (TODO: change to erroring)
pub(crate) fn parse_points_from_array(array: &Value) -> Vec<solver::Point2D> {
    let raw_table = if let Value::Array(a) = array {
        a
    } else {
        eprintln!("Saw a \'{:?}\', but was expecting a toml::Value::Array", array);
        panic!("Attempted to parse points from a non-point array thing!");
    };
    let mut points: Vec<solver::Point2D> = Vec::new();

    for entry in raw_table {
        // maybe a tokenize! macro would be useful
        let mut tokens = {
            let a = if let Value::Array(a) = entry {
                a
            } else {
                todo!(); // maybe a macro to wrap all of this up: the pattern matching as an assert!
            };
            assert!(a.len() >= 2, "Point \'{:?}\' has the wrong length, expected 2 or 4", a);
            a.iter()
        };
        let (id, point_name) = {
            let t = tokens.next().unwrap();
            if let Value::String(s) = t {
                (t.try_into().unwrap(), s.as_str())
            } else {
                todo!()
            }
        };
        let point = match value_to_string(tokens.next().unwrap()) {
            "Origin" => solver::Point2D::origin(id),
            "Cartesian" => {
                let (x, y) = parse_coordinate_pair(tokens, point_name);
                solver::Point2D::cartesian(id, x, y)
            }
            "Polar" => {
                let (r, theta) = parse_coordinate_pair(tokens, point_name);
                solver::Point2D::polar(id, r, theta)
            }
            other => {
                eprintln!("Invalid Point Definition Type!");
                panic!(
                    "Points must be one of [Origin, Cartesian, Polar], but saw \'{}\'",
                    other
                );
            }
        };
        points.push(point);
    }
    points
}
#[allow(unused)]
pub(crate) fn parse_problem(file: String) {
    let data = file.parse::<Table>().unwrap();

    // TODO: validate the problem & parse the problem info from it

    let points = {
        let points_table = match data.get("points") {
            Some(table) => table,
            None => todo!(),
        };
        parse_points_from_array(points_table) // TODO: error handle
    };
}
