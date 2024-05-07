use crate::parsing::ParsingError;
use crate::solver::SolvingError;

mod parsing;
mod solver;
mod tests;

/// Statics problem solver! Solving 2D Trusses in Static Equilibrium
fn main() {
    // get the file we need, skip the program name (first arg)
    let file_path = std::env::args().nth(2).unwrap_or_else(ask_user_for_path);
    let file = match std::fs::read_to_string(&file_path) {
        Ok(f) => f,
        Err(e) => panic!("Error opening file: {:?}", e),
    };

    // startup messages
    let mut info = parsing::get_problem_information(&file);
    println!("..........");
    println!("Statics Problem Solver | Solving the problem at \'{file_path}\'");
    println!("Working on problem: {}!", info.name);
    if info.debug.enabled {
        eprintln!("------------");
        eprintln!("Debug info enabled! The parser & solver will spit out a lot of text!");
        if info.file_write {
            eprintln!(
                "Debug printing to a file is enabled! Results & debug information will be printed to \'answer-{}\'", 
                info.name
            );
        }
        eprintln!("------------");
    }

    let problem = match parsing::parse_problem(file, &mut info.debug) {
        Ok(answer) => answer,
        Err(e) => match e {
            ParsingError::InvalidTOMLFile => panic!("Invalid TOML file provided!"),
            ParsingError::IncorrectPoints(p) => panic!("Invalid Points: {:?}", p),
            // This case was planned, but currently will never trigger
            ParsingError::NotInEquilibrium => {
                panic!("Truss is not in equilibrium, check the problem.")
            }
        },
    };
    let (joints, name_conversion) = (problem.joints, problem.name_map);
    if info.debug.enabled {
        writeln!(info.debug.output, "Name Conversion:").unwrap();
        for (id, name) in name_conversion.iter() {
            writeln!(info.debug.output, "Id {} is \'{}\'", id, name).unwrap();
        }

        writeln!(info.debug.output, "Joints:").unwrap();
        for joint in &joints {
            writeln!(info.debug.output, "{}", joint).unwrap(); // lists the forces acting at the joint
        }
    }

    let solutions = match solver::solve_truss(&joints, &mut info.debug) {
        Ok(answer) => answer,
        Err(SolvingError::NoMatrixWorked) => panic!("No invertible matrix found for this problem!"),
    };

    use std::io::Write;
    for result in solutions {
        let f_dir = if result.value > 0f64 { "T" } else { "C" };

        if info.debug.enabled {
            writeln!(
                info.debug.output,
                "Member {} [{}]: {} ({})",
                name_conversion.get(&result.force).unwrap(),
                result.force,
                result.value,
                f_dir
            )
        } else {
            writeln!(
                info.debug.output,
                "Member {}: {} ({})",
                name_conversion.get(&result.force).unwrap(),
                result.value.abs(),
                f_dir
            )
        }
        .expect("Couldn't write to output!");
    }
    println!("Solving Complete, Program Quitting!");
}

/// Prompts the user to type in a path to a problem.
/// This path is relative to the executable & will trim whitespace.
///
/// If no path is specified, a default one will be used instead.
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
