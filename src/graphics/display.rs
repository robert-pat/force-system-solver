use std::collections::HashMap;

use crate::solver::{Direction2D, Force2D, Point2D, SolverID, TrussJoint2D};

mod render_backing;

enum Support {
    Pin{ id: SolverID},
    Roller{id: SolverID, dir: Direction2D},
}
impl Support {
    fn to_forces(&self, truss: &Truss) -> Vec<Force2D> {
        let mut v = Vec::new();
        match self {
            Support::Pin {id} => {
                let point_name = truss.names.get(id).unwrap();
                let n_x = format!("") 
            }
        }
        v
    }
}

struct ExternalLoad {
    at: SolverID,
    dir: Direction2D,
    mag: f64,
}

struct Truss {
    points: HashMap<SolverID, Point2D>,
    connections: Vec<(SolverID, SolverID)>,
    supports: HashMap<SolverID, Support>,
    external_loads: HashMap<SolverID, Force2D>,
    names: HashMap<SolverID, String>,
}

impl Truss {
    fn condense(&self) -> (Vec<TrussJoint2D>) {
        let mut joints: HashMap<SolverID, TrussJoint2D> = HashMap::new();
        for p in self.points.keys() {
            joints.insert(*p, TrussJoint2D::empty(*p));
        }

        for (id, support) in self.supports.iter() {
            let raw = support.to_forces(&self);
            match joints.get_mut(id) {
                Some(j) => raw.into_iter().for_each(|f| j.add(f)),
                None => panic!("Error condensing display::Truss for solving: support attached at {id} (invalid ID)!")
            }
        }

        todo!()
    }
}

