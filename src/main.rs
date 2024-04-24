use crate::parsing::ParsingError;
use crate::solver::SolvingError;

// TODO: Go through each of thse & write out more doc comments:
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
    println!("..........");
    println!("Statics Problem Solver | Solving the problem at \'{file_path}\'");
    println!("Working on problem: {}!", info.name);
    if info.debug_info {
        eprintln!("------------");
        eprintln!("Debug info enabled! The parser & solver will spit out a lot of text!");
        eprintln!("------------");
        if info.file_write {
            eprintln!("Warning: Debug printing to a file is not yet supported, sorry!");
            eprintln!("Use \'[command] >> name.txt\' to pipe the output to name.txt (windows).");
            eprintln!();
        }
    }

    let problem = match parsing::parse_problem(file, info.debug_info) {
        Ok(answer) => answer,
        Err(e) => match e {
            ParsingError::InvalidTOMLFile => panic!("Invalid TOML file provided!"),
            ParsingError::IncorrectPoints(p) => panic!("Invalid Points: {:?}", p),
            ParsingError::NotInEquilibrium => {
                panic!("Truss is not in equilibrium, check the problem.")
            }
        },
    };
    let (joints, name_conversion) = (problem.joints, problem.name_map);
    if info.debug_info {
        println!("Name Conversion:");
        for (id, name) in name_conversion.iter() {
            println!("Id {} is \'{}\'", id, name);
        }
        println!();

        for joint in &joints {
            println!("{}", joint);
        }
    }

    let solutions = match solver::solve_truss(&joints, info.debug_info) {
        Ok(answer) => answer,
        Err(SolvingError::NoMatrixWorked) => panic!("No invertible matrix found for this problem!"),
    };
    
    use std::io::Write;
    let mut output: Box<dyn Write> = match info.file_write {
        true => {
            let file = std::fs::OpenOptions::new()
                .append(true)
                .write(true)
                .create(true)
                .open(format!("answer-{}", info.name))
                .unwrap();
            Box::new(file)
        }
        false => Box::new(std::io::stdout()),
    };
    for (id, value) in solutions {
        if info.debug_info {
            writeln!(
                output,
                "Member {} [{}]: {} ({})",
                name_conversion.get(&id).unwrap(),
                id,
                if info.debug_info { value } else { value.abs() },
                if value > 0f64 { "T" } else { "C" }
            )
            .expect("Could not write solution to file! Programming Issue.");
        } else {
            writeln!(
                output,
                "Member {}: {} ({})",
                name_conversion.get(&id).unwrap(),
                if info.debug_info { value } else { value.abs() },
                if value > 0f64 { "T" } else { "C" }
            )
            .expect("Could not write solution to file! Programming Issue.");
        }
    }
    println!("Solving Complete, Program Quitting!");
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
        eprintln!();
        return String::from("sample-problems\\problem-one.toml");
    }
    s.trim().to_string() // I know, I know
}
