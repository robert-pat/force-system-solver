use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Formatter;
use std::io::Write;

use itertools::Itertools;
use nalgebra as na;
use toml::{Table, Value};

use crate::solver::{Direction2D, Force2D, Point2D, SolverID, TrussJoint2D, VectorComponent};

/// Holds meta-information about a given problem like its name, whether debug information should
/// be printed, etc.
pub(crate) struct ProblemInformation {
    pub(crate) name: String,
    pub(crate) debug: DebugInfo,
    pub(crate) file_write: bool,
}
impl ProblemInformation {
    pub(crate) fn get(problem: &Table) -> ProblemInformation {
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
    pub(crate) fn display_names(&mut self, names: &HashMap<SolverID, String>) -> Result<(), ()> {
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
#[derive(Clone, Debug, PartialEq)]
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
pub(crate) fn into_points(t: &Value, names: &mut HashMap<SolverID, String>) -> PointsResult {
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
pub(crate) fn into_connections(
    t: &Value,
    names: &mut HashMap<SolverID, String>,
) -> ConnectionsResult {
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
pub(crate) fn into_loads(t: &Value, names: &mut HashMap<SolverID, String>) -> LoadsResult {
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
pub(crate) fn into_supports(t: &Value, names: &mut HashMap<SolverID, String>) -> SupportsResult {
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
