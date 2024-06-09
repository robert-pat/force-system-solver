#![allow(unused)]

use nalgebra as na;

use crate::parsing::DebugInfo;
use itertools::Itertools;
use nalgebra::DMatrix;
use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::fmt::{Debug, Formatter};
use std::hash::Hasher;
use std::ops::Mul;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct SolverID(u64);
impl PartialOrd for SolverID {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.0.cmp(&other.0))
    }
}
impl Ord for SolverID {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}
impl From<&str> for SolverID {
    fn from(value: &str) -> Self {
        SolverID::new(value)
    }
}
impl SolverID {
    pub(crate) fn new(label: &str) -> Self {
        let mut h = DefaultHasher::new();
        h.write(label.as_bytes());
        SolverID(h.finish())
    }
    pub(crate) fn from_data(data: &[u8]) -> Self {
        let mut h = DefaultHasher::new();
        h.write(data);
        SolverID(h.finish())
    }

    /// Concatenates the two IDs together, producing a new ID.
    /// The new ID will be the same for the same given inout IDs
    pub(crate) fn concatenate(self, other: SolverID) -> Self {
        match self.0.cmp(&other.0) {
            Ordering::Equal => self,
            Ordering::Greater => {
                let mut hasher = DefaultHasher::new();
                hasher.write_u64(self.0);
                hasher.write_u64(other.0);
                SolverID(hasher.finish())
            }
            Ordering::Less => {
                let mut hasher = DefaultHasher::new();
                hasher.write_u64(other.0);
                hasher.write_u64(self.0);
                SolverID(hasher.finish())
            }
        }
    }
}
impl std::fmt::Display for SolverID {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}
impl TryFrom<&toml::Value> for SolverID {
    type Error = ();
    fn try_from(value: &toml::Value) -> Result<Self, Self::Error> {
        if let toml::Value::String(s) = value {
            return Ok(SolverID::new(s));
        }
        Err(())
    }
}
#[derive(Clone, Debug)]
pub(crate) struct Point2D {
    x: f64,
    y: f64,
    id: SolverID,
}
impl Point2D {
    pub(crate) fn origin(id: SolverID) -> Self {
        Self {
            id,
            x: 0f64,
            y: 0f64,
        }
    }
    pub(crate) fn polar(id: SolverID, r: f64, theta: f64) -> Self {
        Self {
            id,
            x: r * theta.to_radians().cos(),
            y: r * theta.to_radians().sin(),
        }
    }
    pub(crate) fn cartesian(id: SolverID, x: f64, y: f64) -> Self {
        Self { id, x, y }
    }
    pub(crate) fn has_id(&self, id: &SolverID) -> bool {
        &self.id == id
    }
    pub(crate) fn direction_to(&self, other: &Point2D) -> Direction2D {
        let (dx, dy) = (other.x - self.x, other.y - self.y);
        let mag = (dx.powf(2.0) + dy.powf(2.0)).sqrt();
        Direction2D {
            x: dx / mag,
            y: dy / mag,
        }
    }
    pub(crate) fn distance_to(&self, other: &Point2D) -> f64 {
        let (dx, dy) = (other.x - self.x, other.y - self.y);
        (dx.powf(2.0) + dy.powf(2.0)).sqrt()
    }
    pub(crate) fn id(&self) -> SolverID {
        self.id
    }
    pub(crate) fn pos(&self) -> (f64, f64) {
        (self.x, self.y)
    }
}
impl std::fmt::Display for Point2D {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {}) {}", self.x, self.y, self.id)
    }
}
impl PartialEq for Point2D {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for Point2D {
    fn assert_receiver_is_total_eq(&self) {}
}

/// Represents a unit vector in two dimensions. Because the length is always 1, x and y components
/// of a given vector can always be found by multiplying the vector's magnitude with the x or y
/// of this direction.
#[derive(Debug, Copy, Clone)]
pub(crate) struct Direction2D {
    x: f64,
    y: f64,
}
impl Direction2D {
    fn is_valid(&self) -> bool {
        self.x.is_normal()
            && self.y.is_normal()
            && (self.x.powf(2.0) + self.y.powf(2.0)).sqrt() == 1f64
    }
    pub(crate) fn from_degrees(theta: f64) -> Self {
        Self {
            x: theta.to_radians().cos(),
            y: theta.to_radians().sin(),
        }
    }
}
impl PartialEq for Direction2D {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y
    }
}
impl std::fmt::Display for Direction2D {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "< {}, {} >", self.x, self.y)
    }
}

/// Represents one component of a vector, along any axis.
///
/// This may be a known or unknown value and may have information about whether it is
/// positive or negative. Vector components that are known positive or negative have additional
/// information about their direction, while fully unknown components have no additional info.
#[derive(Debug, Copy, Clone)]
pub(crate) enum VectorComponent {
    /// This component of the vector is unknown and may be positive or negative
    Unknown,
    /// This component of the vector is unknown, but must be positive
    KnownPositive,
    /// This component of the vector is unknown, but must be negative
    KnownNegative,
    /// This component of the vector is known to have the value contained
    KnownExactly(f64),
}

#[derive(Clone, Debug)]
pub(crate) struct Force2D {
    magnitude: VectorComponent,
    direction: Direction2D,
    point: Point2D, // TODO: may at some point replace this with just the point's ID
    id: SolverID,
}
impl Force2D {
    pub(crate) fn new(id: SolverID, p: Point2D, dir: Direction2D, mag: VectorComponent) -> Self {
        Self {
            id,
            point: p,
            magnitude: mag,
            direction: dir,
        }
    }
    pub fn point_id(&self) -> SolverID {
        self.point.id
    }
    pub fn id(&self) -> SolverID {
        self.id
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum EquationCreationError {
    TemplateEmpty,
    NoForcesGiven,
    ForceNotAtJoint(SolverID),
    TemplateDoesNotHaveForce(SolverID),
}
impl std::fmt::Display for EquationCreationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EquationCreationError::TemplateEmpty => write!(f, "Template is empty"),
            EquationCreationError::NoForcesGiven => write!(f, "No forces in joint"),
            EquationCreationError::TemplateDoesNotHaveForce(i) => {
                write!(f, "Error: given template does not have force id {}", i)
            }
            EquationCreationError::ForceNotAtJoint(i) => write!(
                f,
                "Error: joint contains force {} which does not act at its point",
                i
            ),
        }
    }
}
/// Represents the order of unknowns in the rows of the coefficient matrix.
type MatrixRowTemplate = BTreeMap<SolverID, usize>;
/// Represents a full truss (well just it's joints)
type Truss2D = Vec<TrussJoint2D>;

/// A matrix row made from a linear equation of the form: ax1 + bx2 + ... = constant
#[derive(Debug, Clone)]
pub(crate) struct EquationRow {
    coefficients: Vec<f64>,
    constant: f64,
}
impl EquationRow {
    fn unwrap(self) -> (Vec<f64>, f64) {
        (self.coefficients, self.constant)
    }
    fn len(&self) -> usize {
        self.coefficients.len()
    }
}
/// Takes in a set of forces and a template for what order the row vector should be constructed in.
/// The function then creates two row vectors representing the net force equations for the forces
/// in the x and y directions. The last item in each vector is the result of the equation (C) in
/// A * x_1 + B * x_2 + ... = C.
///
/// All of the forces passed in should be acting at the same point / be from the same joint.
/// The rows only contain the coefficients
#[warn(incomplete_features)]
#[allow(unused)]
pub(crate) fn build_equations(
    joint: &TrussJoint2D,
    template: &MatrixRowTemplate,
) -> Result<[EquationRow; 2], EquationCreationError> {
    if joint.forces.is_empty() {
        return Err(EquationCreationError::NoForcesGiven);
    }
    if template.is_empty() {
        return Err(EquationCreationError::TemplateEmpty);
    }
    for f in joint.forces.iter() {
        if f.point_id() != joint.point_id {
            return Err(EquationCreationError::ForceNotAtJoint(f.id));
        }
        if !matches!(VectorComponent::KnownExactly, f) && !template.contains_key(&f.id) {
            return Err(EquationCreationError::TemplateDoesNotHaveForce(f.id));
        }
    }

    let mut x_coefficients = vec![0f64; template.len()];
    let mut y_coefficients = vec![0f64; template.len()];
    let mut x_sum = 0f64;
    let mut y_sum = 0f64;

    for force in joint.forces.iter() {
        if let VectorComponent::KnownExactly(val) = force.magnitude {
            x_sum += val * force.direction.x;
            y_sum += val * force.direction.y;
            continue;
        }

        let key = *template.get(&force.id).unwrap();
        x_coefficients[key] = force.direction.x;
        y_coefficients[key] = force.direction.y;
    }

    // floating point numbers (bc 0 vs non-zero in a matrix matters lots)
    const TOLERANCE: f64 = 0.00000001;
    for v in x_coefficients.iter_mut().chain(y_coefficients.iter_mut()) {
        if (-TOLERANCE..=TOLERANCE).contains(v) {
            *v = 0f64;
        }
    }

    // This is the constant part of the linear equation. In matrix form, it's the value of this row
    // of C in A*B = C. Multiply by -1 bc net force equations are of the form: (sum of things) = 0
    // not (coefficient * variable(s)) = constant
    let x = EquationRow {
        coefficients: x_coefficients,
        constant: x_sum * -1.0,
    };
    let y = EquationRow {
        coefficients: y_coefficients,
        constant: y_sum * -1.0,
    };

    Ok([x, y])
}

#[derive(Debug)]
pub(crate) struct TrussJoint2D {
    pub(crate) point_id: SolverID,
    pub(crate) forces: Vec<Force2D>,
}
impl TrussJoint2D {
    pub fn add(&mut self, force: Force2D) {
        self.forces.push(force)
    }
    pub fn empty(point_id: SolverID) -> Self {
        Self {
            point_id,
            forces: Vec::new(),
        }
    }
}
impl std::fmt::Display for TrussJoint2D {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Joint for point {}:", self.point_id)?;
        for force in &self.forces {
            write!(f, "-> Force of ")?;
            match &force.magnitude {
                VectorComponent::Unknown => write!(f, "Unknown Magnitude "),
                VectorComponent::KnownNegative => write!(f, "Unknown Negative Magnitude "),
                VectorComponent::KnownPositive => write!(f, "Unknown Positive Magnitude "),
                VectorComponent::KnownExactly(mag) => write!(f, "{} ", *mag),
            }?;
            writeln!(f, "@ {} acting on point {}", force.direction, force.point)?;
        }
        Ok(())
    }
}

fn find_unknowns(joints: &Vec<TrussJoint2D>) -> Vec<SolverID> {
    let mut unknowns: Vec<SolverID> = Vec::new();
    for joint in joints {
        for force in &joint.forces {
            if let VectorComponent::KnownExactly(_) = force.magnitude {
                continue;
            }
            unknowns.push(force.id);
        }
    }

    unknowns.sort_unstable();
    unknowns.dedup();
    unknowns
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum SolvingError {
    NoMatrixWorked,
}
#[derive(Debug)]
pub(crate) struct ComputedForce {
    pub(crate) force: SolverID,
    pub(crate) value: f64,
}
impl ComputedForce {
    fn new(id: SolverID, value: f64) -> Self {
        ComputedForce { force: id, value }
    }
}

type SolvingResult = Result<Vec<ComputedForce>, SolvingError>;
pub(crate) fn solve_truss(joints: &Vec<TrussJoint2D>, debug: &mut DebugInfo) -> SolvingResult {
    let row_template: BTreeMap<SolverID, usize> = find_unknowns(joints)
        .into_iter()
        .enumerate()
        .map(|(a, b)| (b, a))
        .collect();
    let num_unknowns = row_template.len();

    if debug.enabled {
        writeln!(
            debug.output,
            "Unknowns the solver is working with (in row order for the matrices):"
        );
        for (u, count) in row_template.iter() {
            writeln!(debug.output, "Unknown {count}: {u}");
        }
        writeln!(debug.output, "End Unknowns ----");
    }

    if num_unknowns > joints.len() * 2 {
        panic!(
            "More unknowns ({}) that available equations ({}), aborting!",
            num_unknowns,
            joints.len() * 2
        );
    };

    let mut potential_rows: Vec<EquationRow> = Vec::new();
    for joint in joints.iter() {
        let equations = build_equations(joint, &row_template).unwrap();
        for equ in equations {
            // can't move out of an array
            assert_eq!(equ.len(), num_unknowns);
            potential_rows.push(equ);
        }
    }

    if debug.enabled {
        writeln!(
            debug.output,
            "Potential Equations [coefficients] = constant:"
        );
        for (count, row) in potential_rows.iter().enumerate() {
            writeln!(
                debug.output,
                "Potential Equation {count}: {:?} = {}",
                row.coefficients, row.constant
            );
        }
    }

    for combination in potential_rows
        .into_iter()
        .enumerate()
        .combinations(num_unknowns)
    {
        let mut coefficients: Vec<f64> = Vec::new(); // could always move these out of loop
        let mut constants: Vec<f64> = Vec::new();
        let mut equations_used: Vec<usize> = Vec::new();

        for (equ_num, row) in combination {
            constants.push(row.constant);
            coefficients.extend(row.coefficients);
            equations_used.push(equ_num);
        }

        let coefficient_matrix =
            na::DMatrix::from_row_slice(num_unknowns, num_unknowns, &coefficients);
        let constant_matrix = na::DMatrix::from_column_slice(constants.len(), 1, &constants);

        assert_eq!(coefficient_matrix.len(), num_unknowns.pow(2));
        assert_eq!(constant_matrix.len(), num_unknowns);

        // .clone() should be worth the debug info like 99.9% of the time
        let inverse = match coefficient_matrix.clone().try_inverse() {
            Some(i) => i,
            None => continue,
        };

        if debug.enabled {
            writeln!(debug.output, "Invertible matrix found!");
            writeln!(debug.output, "With these equations: {:?}", equations_used);

            debug.display_matrix(&coefficient_matrix, "Coefficient Matrix");
            debug.display_matrix(&inverse, "Inverted Matrix");
            debug.display_matrix(&constant_matrix, "Constant Matrix");
        }

        let answer = inverse * constant_matrix;
        if debug.enabled {
            debug.display_matrix(&answer, "Answers");
        }

        let mut return_me = Vec::with_capacity(row_template.len());
        for (id, position) in row_template.into_iter() {
            let force_result = ComputedForce::new(id, *answer.get(position).unwrap());
            return_me.push(force_result);
        }
        return Ok(return_me);
    }
    Err(SolvingError::NoMatrixWorked)
}
