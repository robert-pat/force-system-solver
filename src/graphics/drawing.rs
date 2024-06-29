/* Code for turning things into vertex data. */
use cgmath::{Rotation, vec3};
use crate::parsing::Truss2D;
use crate::display::render_backing::Vertex;
use crate::display::render_backing::NumBytes;
use crate::display::render_backing::NumVertex;

/// Convert a [Truss2D] into a vertex data & write it into the provided slice. This function will
/// not write more than max_size vertices into the buffer. If something goes wrong, the function
/// will return an error and nothing will be written to the provided slice. If max_size is larger
/// than buffer.len(), the function will immediately return.
fn build_vertex_data(truss: &Truss2D, buffer: &mut [Vertex], max: NumVertex) -> Result<NumVertex, ()> {
    if buffer.len() < max || max == 0 {
        return Err(());
    }
    todo!()
}

pub(crate) fn triangle_transform(vertices: &mut [Vertex], max: NumVertex, t_color: bool, rotate: bool) -> Result<NumVertex, ()> {
    if vertices.len() < max || max == 0 {
        return Err(());
    }
    
    let r_transform: cgmath::Basis2<f32> = cgmath::Rotation2::from_angle(cgmath::Rad(30.0_f32.to_radians()));
    for v in vertices.iter_mut() {
        let mut pos = v.pos;
        let mut color = v.color;
        if t_color {
            // each color is f(x0) = (1/2) sin(ax + b) + (1/2) so that w/ x ∈ [0, 1];
            // f(x) ∈ [0,1]. Each color uses the previous one as the next one
            let x = (0.5 * (15.8f32 * color.x + 0f32).sin().abs() + 0.5).clamp(0f32, 1f32);
            let y = (0.5 * (5.9f32 * color.y + 5f32).sin().abs() + 0.5).clamp(0f32, 1f32);
            let z = (0.5 * (8f32 * color.z + 2f32).sin().abs() + 0.5).clamp(0f32, 1f32);
            color = vec3(x, y, z);
        }
        if rotate {
            pos = r_transform.rotate_vector(pos);
        }
        *v = Vertex::new(pos, color);
    }
    Ok(vertices.len())
}