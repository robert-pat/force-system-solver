#![allow(unused)]

/*In the solver, here are the goals:
1) All points have been determined, the only unknown is the magnitude of the loads on the system.
2) All of the loads have been resolved as much as possible, e.g. minimum unknowns
3) Everything (point, force, couple moment) has an id to refer to other things
*/

use std::fmt::Formatter;

/// Chunk of a statics problem that can be solved by applying equations of equilibrium.
/// These hold forces, couple moments, and points that this body includes; all of its unknowns can be found
/// by applying equations of equilibrium:
///
///   ΣF_x = 0   ΣF_y = 0   ΣF_z = 0  &  ΣM_(any point) = 0
///
/// This gets converted into a matrix equation that gets solved by {LINEAR_ALGEBRA_LIBRARY}
pub(crate) struct RigidBody {
    points: Vec<Point3D>,
    forces: Vec<Force3D>,
    moments: Vec<CoupleMoment3D>,
    // TODO: what does this struct need to hold?
}
impl RigidBody {
    /* TODO: Write the solver function here:
        - Need to find an appropriate linear algebra library
        - Need to set up the matrix w/ appropriate numbers of equations
        - Need to input values from those equations into the matrix
    */
    pub(crate) fn has_point(&self, point: &Point3D) -> bool {
        self.points.contains(point)
    }
    pub(crate) fn count_unknowns(&self) -> u32 {
        let mut count = 0;
        for force in &self.forces {
            for comp in [&force.x_i, &force.y_j, &force.z_k] {
                if !matches!(comp, VectorComponent::KnownExactly(_)) {
                    count += 1;
                }
            }
        }
        for moment in &self.moments {
            for comp in [&moment.x_i, &moment.y_j, &moment.z_k] {
                if !matches!(comp, VectorComponent::KnownExactly(_)) {
                    count += 1;
                }
            }
        }
        count
    }
}
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
struct SolverID(u64);
impl std::fmt::Display for SolverID {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}
#[derive(Clone, Debug)]
pub(crate) struct Point3D {
    x: f64,
    y: f64,
    z: f64,
    id: SolverID,
}
impl PartialEq for Point3D {
    fn eq(&self, other: &Self) -> bool {
        for (x, y) in [(self.x, other.x), (self.y, other.y), (self.z, other.z)] {
            if x.is_normal() && y.is_normal() {
                assert_eq!(
                    x, y,
                    "Points {} and {} disagreed on a coordinate: {x} and {y}",
                    self.id, other.id
                );
            }
        }
        self.id == other.id
    }
}
impl Eq for Point3D {
    fn assert_receiver_is_total_eq(&self) {}
}
pub(crate) struct Force3D {
    x_i: VectorComponent,
    y_j: VectorComponent,
    z_k: VectorComponent,
    origin: Point3D,
    id: SolverID,
}
pub(crate) struct CoupleMoment3D {
    x_i: VectorComponent,
    y_j: VectorComponent,
    z_k: VectorComponent,
    id: SolverID,
}
/// Represents one component of a vector, along any axis.
///
/// This may be a known or unknown value and may have information about whether it is
/// positive or negative
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
impl VectorComponent {
    fn make_exact_safe(value: f64) -> Option<Self> {
        if value.is_normal() {
            Some(VectorComponent::KnownExactly(value))
        } else {
            None
        }
    }
}
impl PartialEq for VectorComponent {
    fn eq(&self, other: &Self) -> bool {
        if let VectorComponent::KnownExactly(v1) = self {
            if !v1.is_normal() {
                panic!("Floating-point error in the length of a vector component {v1}! (Tried to compare components)");
            }
            if let VectorComponent::KnownExactly(v2) = other {
                return v1 == v2;
            }
        }
        matches!(self, other)
    }
}
impl Eq for VectorComponent {
    fn assert_receiver_is_total_eq(&self) {}
}
