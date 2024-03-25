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
struct SolverID(u64);
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
}
impl std::fmt::Display for SolverID {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}
#[derive(Clone, Debug)]
pub(crate) struct Point2D {
    x: f64,
    y: f64,
    id: SolverID,
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
}
impl PartialEq for Direction2D {
    fn eq(&self, other: &Self) -> bool {
        // technically because these should always have their magnitude at 1, comparing both
        // is unnecessary I think.
        self.x == other.x && self.y == other.y
    }
}

/// Represents a real number. Currently, this is a wrapper for the
/// f64 type, with some added code to deal with NaN, infinity, ect.
/// The goal for this solver is to be accurate, so this is a temporary measure
/// in the case that floats prove too finicky (NOT Currently Used--Planned Future)
#[allow(unused)]
#[derive(Debug, Copy, Clone)]
pub(crate) struct Number(f64);

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
    fn x(&self) -> VectorComponent {
        match &self.magnitude {
            VectorComponent::KnownExactly(val) => {
                VectorComponent::KnownExactly(val * self.direction.x)
            }
            _ => VectorComponent::Unknown,
        }
    }
    fn y(&self) -> VectorComponent {
        match &self.magnitude {
            VectorComponent::KnownExactly(val) => {
                VectorComponent::KnownExactly(val * self.direction.y)
            }
            _ => VectorComponent::Unknown,
        }
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
fn get_rows_from_joint(
    forces: &[Force2D],
    template: &[SolverID],
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

struct TrussJoint2D {
    point: Point2D,
    forces: Vec<Force2D>,
}

fn count_unknowns(joints: &Vec<TrussJoint2D>) -> usize {
    let mut set = BTreeSet::new();
    for joint in joints {
        for force in &joint.forces {
            set.insert(force.id);
        }
    }
    set.len()
}

fn solve_truss(joints: &Vec<TrussJoint2D>) -> () {
    let num_unknowns = count_unknowns(joints);
    if num_unknowns > joints.len() * 2 {
        todo!()
    };

    let order = [SolverID::new("1")]; // TODO
    let mut coefficient_elems = Vec::new();
    let mut others: Vec<f64> = Vec::new();

    for joint in joints {
        //TODO: make sure after popping these are the right len for when we make the matrix

        let (mut x, mut y) = get_rows_from_joint(&joint.forces, &order).unwrap();
        others.push(x.pop().unwrap());
        others.push(y.pop().unwrap());

        coefficient_elems.extend(x);
        coefficient_elems.extend(y);
    }

    let mut m = na::DMatrix::from_row_slice(num_unknowns, num_unknowns, &coefficient_elems);
    let mut b = na::DMatrix::from_column_slice(num_unknowns, 1, &others);

    let inverse = match m.try_inverse() {
        Some(i) => i,
        None => panic!(),
    };

    let answers = inverse * b;
    todo!()
}
