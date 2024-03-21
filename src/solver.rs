#![allow(unused)]

/*In the solver, here are the goals:
    TODO: goals & expectations + update the README
*/

use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::fmt::Formatter;
use std::hash::Hasher;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
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
impl Point2D {
    pub(crate) fn direction_to(other: &Point2D) -> Direction2D {
        todo!()
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

/// Represents a unit vector in two dimensions. Because the vector's length is 1,
/// this is effectively just a direction
#[derive(Debug, Copy, Clone)]
pub(crate) struct Direction2D {
    x: f64,
    y: f64,
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
impl PartialEq for Force2D {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for Force2D {
    fn assert_receiver_is_total_eq(&self) {}
}
impl PartialOrd for Force2D {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.id.cmp(&other.id))
    }
}
impl Ord for Force2D {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
}
impl Force2D {
    fn x(&self) -> VectorComponent {
        match &self.magnitude {
            VectorComponent::KnownExactly(val) => VectorComponent::KnownExactly(val * self.direction.x),
            _ => VectorComponent::Unknown,
        }
    }
    fn y(&self) -> VectorComponent {
        match &self.magnitude {
            VectorComponent::KnownExactly(val) => VectorComponent::KnownExactly(val * self.direction.y),
            _ => VectorComponent::Unknown,
        }
    }
}


#[warn(incomplete_features)]
#[allow(unused)]
fn build_equations(forces: &[Force2D], template: &[SolverID]) -> Result<[Vec<f64>; 2], ()> {
    // TODO: Validation, and / or make sure it happens before here
    
    // TODO: how to ensure the matrix rows are build in the same order that the template has?
    let mut forces = forces.to_vec();
    forces.sort();
    
    // TODO: Proof of concept, bc we store the magnitude & direction, its easy to build matrix
    //  coefficients bc the coefficient is just the magnitude info
    let mut x_coefficients: Vec<f64> = Vec::with_capacity(template.len());
    let mut x_sum = 0f64;
    for force in forces {
        if let VectorComponent::KnownExactly(val) = force.magnitude {
            x_sum += val * force.direction.x;
            continue;
        }
        
        x_coefficients.push(force.direction.x);
    }
    
    todo!()
}