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
use crate::solver;
use crate::solver::SolverID;
use std::collections::BTreeSet;
use std::slice::Iter;
use toml::{Table, Value};

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

macro_rules! toml_to_float {
    ($x: expr) => {
        match x.next()
        Some(Value::Float(f)) => *f,
        Some(val) => {
            eprintln!("Invalid coordinates for point \'{point_name}\'!");
            eprintln!("Expected a toml::Value::Float, but saw {:?} instead.", val);
            panic!("Error! see above.");
        }
        None => {
            eprintln!("Missing Coordinates for Point \'{point_name}\'!");
            eprintln!("Expected two decimals for the point's position, saw nothing.");
            panic!("Error! see above.");
        }
    }
}
fn to_f64_pair(mut value: Iter<Value>, point_name: &str) -> (f64, f64) {
    // TODO: This macro is only inside this function bc I'm not 100% confident with it yet
    let a = match value.next() {
        Some(Value::Float(f)) => *f,
        Some(val) => {
            eprintln!("Invalid coordinates for point \'{point_name}\'!");
            eprintln!("Expected a toml::Value::Float, but saw {:?} instead.", val);
            panic!("Error! see above.");
        }
        None => {
            eprintln!("Missing Coordinates for Point \'{point_name}\'!");
            eprintln!("Expected two decimals for the point's position, saw nothing.");
            panic!("Error! see above.");
        }
    };
    let b = match value.next() {
        Some(Value::Float(f)) => *f,
        Some(val) => {
            eprintln!("Invalid coordinates for point \'{point_name}\'!");
            eprintln!("Expected a toml::Value::Float, but saw {:?} instead.", val);
            panic!("Error! see above.");
        }
        None => {
            eprintln!("Missing Coordinates for Point \'{point_name}\'!");
            eprintln!("Expected two decimals for the point's position, saw nothing.");
            panic!("Error! see above.");
        }
    };
    if value.len() != 0 {
        eprintln!("Warning, Point declarations should end with 2 coordinates only!");
        eprintln!(
            "Saw a coordinate pair with {} extra items, should be 0!",
            value.len()
        );
        eprintln!(
            "Expected: \"{a}\", \"{b}\" but Saw: \"{a}\", \"{b}\", \"{}\"... <- Extra!",
            value.next().unwrap()
        );
    }
    (a, b)
}
fn value_to_array(value: &Value) -> &Vec<Value> {
    if let Value::Array(a) = value {
        return a;
    }
    eprintln!("Can not convert non-array type into array!");
    eprintln!(
        "Expected to find a toml::Value::Array, but saw \'{:?}\' instead.",
        value
    );
    eprintln!("Unable to unwrap into an array.");
    panic!("Invalid Type, see above!");
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum DuplicatePointType {
    Origin,
    Name,
    Location,
}
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum MalformedPointType {
    InvalidName,
    IncorrectLength,
}
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PointsParsingError {
    Duplicate(DuplicatePointType),
    Malformed(MalformedPointType),
}
/// Takes in a toml::Value::Array and parses it into an array of Point2D for the
/// solver to use. The function signature doesn't capture this, but the Value passed in
/// must be the Value::Array(_) variant, or the function will panic (TODO: change to erroring)
pub(crate) fn array_to_points(array: &Value) -> Result<Vec<solver::Point2D>, PointsParsingError> {
    let raw_table = value_to_array(array);
    let mut points: Vec<solver::Point2D> = Vec::new();

    // TODO: also need to validate that points have unique names & locations!!
    let mut seen_origin = false;
    let mut seen_ids: BTreeSet<SolverID> = BTreeSet::new();

    for entry in raw_table {
        let mut tokens = {
            let a = value_to_array(entry);
            if !(a.len() == 2 || a.len() == 4) {
                /*TODO: will we ever be in a situation to recover from errors like this?
                bc making a useful error type is only helpful if we can recover from them
                later i.e. in the calling function. I don't think we can, so it should panic
                here, where we can provide the most context as possible before exiting. */
                return Err(PointsParsingError::Malformed(
                    MalformedPointType::IncorrectLength,
                ));
            }
            a.iter()
        };
        let id: SolverID = match tokens.next().unwrap().try_into() {
            Ok(id) => id,
            Err(_) => todo!(),
        };
        let point = match value_to_string(tokens.next().unwrap()) {
            "Origin" if !seen_ids.contains(&id) => {
                if seen_origin {
                    panic!("")
                }
                solver::Point2D::origin(id)
            }
            "Cartesian" if !seen_ids.contains(&id) => {
                let (x, y) = to_f64_pair(tokens, "");
                solver::Point2D::cartesian(id, x, y)
            }
            "Polar" if !seen_ids.contains(&id) => {
                let (r, theta) = to_f64_pair(tokens, "");
                solver::Point2D::polar(id, r, theta)
            }
            other => {
                eprintln!("Invalid Point Definition Type!");
                eprintln!(
                    "Points must be one of [Origin, Cartesian, Polar], but saw \'{}\'",
                    other
                );
                panic!("Unable to parse file! See previous errors")
            }
        };
        points.push(point);
    }
    Ok(points)
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
        array_to_points(points_table).unwrap() // TODO: error handle
    };
}
