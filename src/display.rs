use crate::solver::{Direction2D, Force2D, Point2D, SolverID};

enum Support {
    Pin(SolverID),
    Roller(SolverID, Direction2D),
}

struct ExternalLoad {
    at: SolverID,
    dir: Direction2D,
    mag: f64,
}

struct Truss {
    points: Vec<Point2D>,
    connections: Vec<(SolverID, SolverID)>,
    supports: Vec<Support>,
    external_loads: Vec<Force2D>
}

