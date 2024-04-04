#![allow(unused)]

use nalgebra as na;

use nalgebra::DMatrix;
use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, BTreeSet, HashMap};
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

#[derive(Clone)]
pub(crate) struct Force2D {
    magnitude: VectorComponent,
    direction: Direction2D,
    point: Point2D,
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
    pub fn point(&self) -> &Point2D {
        &self.point
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
pub(crate) fn get_rows_from_joint(
    forces: &[Force2D],
    template: &[SolverID], //TODO: should this be changed to &BTreeMap<SolverID, usize> ?
) -> Result<(Vec<f64>, Vec<f64>), ()> {
    if forces.is_empty() || template.is_empty() {
        return Err(());
    }
    let point = forces[0].point.clone();
    if forces.iter().any(|f| f.point != point) {
        return Err(());
    }
    if forces.iter().any(|f| !template.contains(&f.id)) {
        return Err(());
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

pub(crate) struct TrussJoint2D {
    point: Point2D,
    forces: Vec<Force2D>,
}
impl TrussJoint2D {
    pub(crate) fn new(p: Point2D, forces: Vec<Force2D>) -> Option<Self> {
        if forces.iter().any(|f| f.point != p) {
            return None;
        }
        Some(Self { point: p, forces })
    }
    pub fn add(&mut self, force: Force2D) {
        self.forces.push(force)
    }
}

fn find_unknowns(joints: &Vec<TrussJoint2D>) -> Vec<SolverID> {
    let mut set = BTreeSet::new();
    for joint in joints {
        for force in &joint.forces {
            set.insert(force.id);
        }
    }
    let mut unknowns = set.into_iter().collect::<Vec<_>>();
    unknowns.sort();
    unknowns
}

fn solve_truss(joints: &Vec<TrussJoint2D>) -> Result<Vec<(SolverID, f64)>, ()> {
    let unknowns = find_unknowns(joints);
    let num_unknowns = unknowns.len();
    if num_unknowns > joints.len() * 2 {
        // The solution can't be fully constrained
        todo!()
    };

    // TODO: we can't just use any equations arbitrarily:
    //  We add equations so that each unknown has at least 1 non-zero spot
    //  then we just add whatever equations we have
    //  see .iter().cycle() and just keep track of what's been added so far
    let mut coefficients = Vec::new();
    let mut constants: Vec<f64> = Vec::new();

    for joint in joints {
        let (mut x, mut y) = get_rows_from_joint(&joint.forces, &unknowns).unwrap();

        assert_eq!(x.len(), num_unknowns + 1); // TODO: maybe should not just assert?
        assert_eq!(y.len(), num_unknowns + 1);

        constants.push(x.pop().unwrap());
        constants.push(y.pop().unwrap());

        coefficients.extend(x);
        coefficients.extend(y);
    }

    assert_eq!(coefficients.len(), num_unknowns.pow(2));
    assert_eq!(constants.len(), num_unknowns);

    let mut m = na::DMatrix::from_row_slice(num_unknowns, num_unknowns, &coefficients);
    let mut b = na::DMatrix::from_column_slice(num_unknowns, 1, &constants);

    let inverse = match m.try_inverse() {
        Some(i) => i,
        None => panic!(),
    };

    let answers = inverse * b;
    Ok(unknowns
        .into_iter()
        .zip(answers.iter().copied())
        .collect::<Vec<_>>())
}
