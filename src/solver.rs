#![allow(unused)]

/*In the solver, here are the goals:
1) All points have been determined, the only unknown is the magnitude of the loads on the system.
2) All of the loads have been resolved as much as possible, e.g. minimum unknowns
3) Everything (point, force, couple moment) has an id to refer to other things
*/

/// Chunk of a statics problem that can be solved by applying equations of equilibrium.
/// These hold forces, couple moments, and points that this body includes; all of its unknowns can be found
/// by applying equations of equilibrium:
///
///   ΣF_x = 0   ΣF_y = 0   ΣF_z = 0  &  ΣM_(any point) = 0
/// 
/// This gets converted into a matrix equation that gets solved by {LINEAR_ALGEBRA_LIBRARY}
struct RigidBody {
    // TODO: what does this struct need to hold?
}