use std::collections::HashMap;

use crate::solver::{Direction2D, Force2D, Point2D, SolverID, TrussJoint2D};

mod render_backing;

#[derive(Debug)]
enum Support {
    Pin{ id: SolverID},
    Roller{id: SolverID, dir: Direction2D},
}
impl Support {
    fn to_forces(&self, truss: &Truss) -> Vec<Force2D> {
        let mut v = Vec::new();
        todo!();
        v
    }
}

struct ExternalLoad {
    at: SolverID,
    dir: Direction2D,
    mag: f64,
}
// TODO: do I want this or is it disgusting ?
#[derive(Default, Debug)]
pub(crate) struct TrussBuilder {
    points: HashMap<SolverID, Point2D>,
    connections: Vec<(SolverID, SolverID)>,
    supports: HashMap<SolverID, Support>,
    external_loads: HashMap<SolverID, Force2D>,
    names: HashMap<SolverID, String>,
}
impl TrussBuilder {
    pub(crate) fn new() -> Self {
        Self { ..Default::default() }
    }
    pub(crate) fn points(mut self, points: HashMap<SolverID, Point2D>) -> Self {
        self.points = points;
        self
    }
    pub(crate) fn connections(mut self, connections: Vec<(SolverID, SolverID)>) -> Self {
        self.connections = connections;
        self
    }
    // TODO: fix this, how much do we want the parser to see
    pub(crate) fn supports(mut self, supports: HashMap<SolverID, Force2D>) -> Self {
        // TODO: fix also
        self
    }
    pub(crate) fn external_loads(mut self, e_l: HashMap<SolverID, Force2D>) -> Self {
        self.external_loads = e_l;
        self
    }
    pub(crate) fn names(mut self, names: HashMap<SolverID, String>) -> Self {
        self.names = names;
        self
    }
    pub(crate) fn build(self) -> Truss {
        Truss {
            points: self.points,
            connections: self.connections,
            supports: self.supports,
            external_loads: self.external_loads,
            names: self.names,
        }
    }
}

pub(crate) struct Truss {
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

