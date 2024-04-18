#![allow(unused)]

use nalgebra as na;

use itertools::Itertools;
use nalgebra::DMatrix;
use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::fmt::Formatter;
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
    /// TODO: Hash collision potential??
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
    pub(crate) fn coords(&self) -> (f64, f64) {
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
    pub(crate) fn from_angle(theta: f64) -> Self {
        Self {
            x: theta.to_radians().cos(),
            y: theta.to_radians().sin(),
        }
    }
}
impl PartialEq for Direction2D {
    fn eq(&self, other: &Self) -> bool {
        // technically because these should always have their magnitude at 1, comparing both
        // is unnecessary I think.
        self.x == other.x && self.y == other.y
    }
}
impl std::fmt::Display for Direction2D {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}, {}>", self.x, self.y)
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
pub(crate) enum JointToEquationError {
    ForceHasDifferentPoint,
    TemplateEmpty,
    NoForcesGiven,
    TemplateDoesNotHaveForce,
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
pub(crate) fn get_rows_from_joint(
    forces: &[Force2D],
    template: &[SolverID], //TODO: should this be changed to &BTreeMap<SolverID, usize> ?
) -> Result<(Vec<f64>, Vec<f64>), JointToEquationError> {
    if forces.is_empty() {
        return Err(JointToEquationError::NoForcesGiven);
    }
    if template.is_empty() {
        return Err(JointToEquationError::TemplateEmpty);
    }
    let point = forces[0].point.clone();
    if forces.iter().any(|f| f.point != point) {
        return Err(JointToEquationError::ForceHasDifferentPoint);
    }
    if forces.iter().any(|f| {
        !matches!(f.magnitude, VectorComponent::KnownExactly(_)) && !template.contains(&f.id)
    }) {
        return Err(JointToEquationError::TemplateDoesNotHaveForce);
    }

    let mut keys: BTreeMap<SolverID, usize> = {
        let mut map = BTreeMap::new();
        for (index, id) in template.iter().enumerate() {
            map.insert(*id, index);
        }
        map
    };

    let (mut x_coefficients, mut y_coefficients) =
        (vec![0f64; template.len()], vec![0f64; template.len()]);
    let (mut x_sum, mut y_sum) = (0f64, 0f64);

    for force in forces {
        if let VectorComponent::KnownExactly(val) = force.magnitude {
            x_sum += val * force.direction.x;
            y_sum += val * force.direction.y;
            continue;
        }

        let key = *keys.get(&force.id).unwrap();
        x_coefficients[key] = force.direction.x;
        y_coefficients[key] = force.direction.y;
    }

    // This is the constant part of the linear equation. In matrix form, it's the value of this row
    // of C in A*B = C. Multiply by -1 bc net force equations are of the form: (sum of things) = 0
    // not (coefficient * variable(s)) = constant
    x_coefficients.push(-1.0 * x_sum);
    y_coefficients.push(-1.0 * y_sum);

    Ok((x_coefficients, y_coefficients))
}

#[derive(Debug)]
pub(crate) struct TrussJoint2D {
    point_id: SolverID,
    forces: Vec<Force2D>,
}
impl TrussJoint2D {
    pub(crate) fn new(p: Point2D, forces: Vec<Force2D>) -> Option<Self> {
        if forces.iter().any(|f| f.point != p) {
            return None;
        }
        Some(Self {
            point_id: p.id,
            forces,
        })
    }
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

    unknowns.sort();
    unknowns.dedup();
    unknowns
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum SolvingError {
    NoMatrixWorked,
}
pub(crate) fn solve_truss(
    joints: &Vec<TrussJoint2D>,
) -> Result<Vec<(SolverID, f64)>, SolvingError> {
    let unknowns = find_unknowns(joints);
    let num_unknowns = unknowns.len();

    if num_unknowns > joints.len() * 2 {
        // The solution can't be fully constrained
        todo!()
    };

    let mut potential_rows: Vec<Vec<f64>> = Vec::new();
    for joint in joints {
        let (mut x, mut y) = get_rows_from_joint(&joint.forces, &unknowns).unwrap();

        assert_eq!(x.len(), num_unknowns + 1);
        assert_eq!(y.len(), num_unknowns + 1);

        // floating point numbers (bc 0 vs non-zero in a matrix matters lots)
        const TOLERANCE: f64 = 0.00000001;
        for v in x.iter_mut() {
            if (-TOLERANCE..=TOLERANCE).contains(v) {
                *v = 0f64;
            }
        }
        for v in y.iter_mut() {
            if (-TOLERANCE..=TOLERANCE).contains(v) {
                *v = 0f64;
            }
        }

        potential_rows.push(x);
        potential_rows.push(y);
    }

    for combination in potential_rows.iter().combinations(num_unknowns) {
        let mut coefficients: Vec<f64> = Vec::new();
        let mut constants: Vec<f64> = Vec::new();
        for row in combination {
            let mut row = row.to_owned();
            constants.push(row.pop().unwrap());
            coefficients.extend(row);
        }

        let coefficient_matrix =
            na::DMatrix::from_row_slice(num_unknowns, num_unknowns, &coefficients);
        let constant_matrix = na::DMatrix::from_column_slice(constants.len(), 1, &constants);

        assert_eq!(coefficient_matrix.len(), num_unknowns.pow(2));
        assert_eq!(constant_matrix.len(), num_unknowns);

        let inverse = match coefficient_matrix.try_inverse() {
            Some(i) => i,
            None => {
                // TODO: print out the failed matrices in debug mode
                // #[cfg(debug_assertions)]
                // println!("Debug assertion! Tried to invert matrix {:?}", coefficient_matrix);
                continue;
            }
        };
        let answer = inverse * constant_matrix;
        return Ok(unknowns
            .into_iter()
            .zip(answer.iter().copied())
            .collect::<Vec<_>>());
    }
    panic!();
}
