use std::collections::BTreeMap;

mod parsing;
mod solver;
mod tests;

/// Statics problem solver
fn main() {
    // get the file we need, skip the program name (arg 1)
    let file_path = std::env::args().nth(2).unwrap_or_else(ask_user_for_path);
    let file = match std::fs::read_to_string(&file_path) {
        Ok(f) => f,
        Err(e) => panic!("Error opening file: {:?}", e),
    };

    // startup
    let info = parsing::get_problem_information(&file);
    println!("Statics Problem Solver | Solving the problem at \'{file_path}\'");
    println!("Working on problem: {}!", info.name);
    if info.debug_info {
        eprintln!("Debug info enabled! The parser & solver will spit out a lot of text!");
    }

    let parsing_result = match parsing::parse_problem(file, info.debug_info) {
        Ok(answer) => answer,
        Err(_) => todo!(),
    };
    // TODO: build up the map between SolverID and actual name in text
    let id_to_name: BTreeMap<solver::SolverID, String> = BTreeMap::new();

    let solutions = match solver::solve_truss(&parsing_result.joints) {
        Ok(answer) => answer,
        Err(_) => todo!(),
    };

    use std::io::Write;
    let mut output = std::fs::OpenOptions::new()
        .append(true)
        .write(true)
        .create(true)
        .open(format!("answer-{}", info.name))
        .unwrap();
    for (id, value) in solutions {
        write!(
            output,
            "{}: {}({})",
            id, //id_to_name.get(&id).unwrap(),
            value,
            if value > 0f64 { "C" } else { "T" }
        )
        .expect("");
    }
}

fn ask_user_for_path() -> String {
    String::from("sample-problems\\problem-one.toml")
}
