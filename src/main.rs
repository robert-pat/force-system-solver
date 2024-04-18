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
        if info.file_write {
            eprintln!("Warning: Debug printing to a file is not yet supported, sorry!");
            eprintln!("Use \'[command] >> name.txt\' to pipe the output to name.txt (windows).");
        }
    }

    let problem = match parsing::parse_problem(file, info.debug_info) {
        Ok(answer) => answer,
        Err(_) => todo!(),
    };
    let (joints, name_conversion) = (problem.joints, problem.name_map);

    let solutions = match solver::solve_truss(&joints) {
        Ok(answer) => answer,
        Err(_) => todo!(),
    };

    // TODO: I don't love how this code is, but its fine for now
    use std::io::Write;
    if info.file_write {
        let mut output = std::fs::OpenOptions::new()
            .append(true)
            .write(true)
            .create(true)
            .open(format!("answer-{}", info.name))
            .unwrap();
        for (id, value) in solutions {
            writeln!(
                output,
                "Member {} [{}]: {}({})",
                name_conversion.get(&id).unwrap(),
                id,
                value,
                if value > 0f64 { "C" } else { "T" }
            )
            .expect("Could not write solution to file! Programming Issue.");
        }
    } else {
        for (id, value) in solutions {
            println!(
                "Member {} [{}]: {}({})",
                name_conversion.get(&id).unwrap(),
                id,
                value,
                if value > 0f64 { "C" } else { "T" }
            );
        }
    }
}

fn ask_user_for_path() -> String {
    println!("Please enter a file path to the problem you would like solved.");
    println!("This path should be relative to the current directory:");
    let mut s = String::new();
    std::io::stdin()
        .read_line(&mut s)
        .expect("Failed to read std in! Programming Issue.");

    // if any of the characters are not whitespace, that's a path, otherwise default
    if !s.chars().any(|c| !c.is_ascii_whitespace()) {
        eprintln!("Entered file path is empty, using a sample problem!");
        eprintln!("Expecting the sample problem: \'sample-problems\\problem-one.toml\'");
        return String::from("sample-problems\\problem-one.toml");
    }
    s
}
