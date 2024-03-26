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
use toml::{Table, Value};

#[warn(incomplete_features)]
fn _string(value: &Value) -> &str {
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
#[warn(incomplete_features)]
fn _double(value: &Value) -> f64 {
    if let Value::Float(s) = value {
        return *s;
    }
    eprintln!("Can not convert non-floating point value to f64!");
    eprintln!(
        "Expected to find a toml::Value::Float, but saw \'{:?}\' instead",
        value
    );
    panic!("Invalid type, see above error");
}
/// Takes in a toml::Value::Array and parses it into an array of Point2D for the
/// solver to use. The function signature doesn't capture this, but the Value passed in
/// must be the Value::Array(_) variant, or the function will panic (TODO: change to erroring)
fn array_to_points(array: &Value) -> Vec<solver::Point2D> {
    let raw_table = match array {
        Value::Array(a) => a,
        other => {
            eprintln!("Internal Error: attempted to parse points from a non-array variant");
            eprintln!(
                "Expected toml::Value::Array, but saw \'{:?}\' instead",
                other
            );
            eprintln!("Was attempting to create an array of unprocessed points.");
            panic!("Internal Error, see above.")
        }
    };

    let mut points: Vec<solver::Point2D> = Vec::new();
    for entry in raw_table {
        let mut tokens = match entry {
            Value::Array(a) => {
                if !(a.len() == 2 || a.len() == 4) {
                    eprintln!("Incorrectly-declared point!");
                    eprintln!("Points must have [name, Origin] or [name, type, val1, val2]");
                    eprintln!("Saw a point \'{:?}\' with a length of {}, expected 2 or 4", a, a.len());
                    panic!("Error processing points, see above.");
                }
                a.iter()
            }
            other => {
                eprintln!("Internal Error: attempted to parse points from a non-array variant");
                eprintln!(
                    "Expected toml::Value::Array, but saw \'{:?}\' instead",
                    other
                );
                eprintln!("Was attempting to break an unprocessed point into array");
                panic!("Internal Error, see above.")
            }
        };
        let id: solver::SolverID = match tokens.next().unwrap().try_into() {
            Ok(id) => id,
            Err(_) => todo!(),
        };
        let point = match _string(tokens.next().unwrap()) {
            "Origin" => solver::Point2D::origin(id),
            "Cartesian" => {
                let (x, y) = (
                    _double(tokens.next().unwrap()),
                    _double(tokens.next().unwrap()),
                );
                solver::Point2D::cartesian(id, x, y)
            }
            "Polar" => {
                let (r, theta) = (
                    _double(tokens.next().unwrap()),
                    _double(tokens.next().unwrap()),
                );
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
    points
}
#[allow(unused)]
pub(crate) fn parse_problem(file: String) {
    let data = file.parse::<Table>().unwrap();
    
    // TODO: validate the problem & parse the problem info from it
    
    let points = array_to_points(match data.get("points") {
        Some(val) => val,
        None => todo!(),
    });
}
