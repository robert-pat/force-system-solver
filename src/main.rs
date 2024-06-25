use crate::parsing::ParsingError;

mod parsing;
mod solver;
mod tests;
#[path = r"graphics\display.rs"]
mod display;

/// Statics problem solver! Solving 2D Trusses in Static Equilibrium
fn main() {
    // ---- Experimental Changes -----
    if std::env::args().any(|a| a.as_str() == "-n") {
        use CommandLineProgramError as CLPE;
        match use_new_parser(ask_user_for_path()) {
            Err(CLPE::Solving(_e)) => {
                eprintln!("Error in the solver:");
                panic!("{_e:?}");
            },
            Err(CLPE::Parsing(_e)) => {
                eprintln!("Error parsing file from toml:");
                panic!("{_e:?}");
            },
            Err(CLPE::FileIO(_e)) => {
                eprintln!("Error with FileIO:");
                panic!("{_e}");
            },
            Err(CLPE::Conversion(_e)) => {
                eprintln!("Error converting to Truss");
                panic!("{_e}");
            },
            Ok(_) => {},
        };
        return;
    }


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
        eprintln!("------------");
    }
    if info.file_write {
        eprintln!("File writing is enabled! Output (debug & results) will be printed to \'answer-{}\'.txt", info.name);
    }

    let (joints, names) = match parsing::parse_problem(file, &mut info.debug) {
        Ok(r) => (r.joints, r.name_map),
        Err(e) => match e {
            ParsingError::InvalidTOMLFile(t) => {
                panic!("Couldn't parse the toml file; {}", t.message())
            }
            ParsingError::MissingPointsTable => panic!("File did not contain a points table!"),
            ParsingError::MissingLoadsTable => {
                panic!("File did not contain a table of applied loads!")
            }
            ParsingError::MissingSupportsTable => {
                panic!("File did not contain a table for supports!")
            }
            ParsingError::MissingMembersTable => {
                panic!("File did not contain a table for truss members!")
            }
        },
    };
    if info.debug.enabled {
        writeln!(info.debug.output, "Name Conversion:").unwrap();
        for (id, name) in names.iter() {
            writeln!(info.debug.output, "Id {} is \'{}\'", id, name).unwrap();
        }

        writeln!(info.debug.output, "Joints:").unwrap();
        for joint in &joints {
            writeln!(info.debug.output, "{}", joint).unwrap(); // lists the forces acting at the joint
        }
    }

    let solutions = solver::solve_truss(&joints, &mut info.debug)
        .expect("No invertible matrix found for this problem!");

    use std::io::Write;
    for result in solutions {
        let dir = if result.value > 0f64 { "T" } else { "C" };
        let name = names.get(&result.force).unwrap();

        if info.debug.enabled {
            writeln!(
                info.debug.output,
                "Member {} [{}]: {} ({dir})",
                name, result.force, result.value,
            )
        } else {
            writeln!(
                info.debug.output,
                "Member {}: {:.8} ({dir})",
                name,
                result.value.abs(),
            )
        }
        .expect("Couldn't write to output!");
    }
    writeln!(info.debug.output, "Solving Complete, Program Quitting!").unwrap();
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

enum CommandLineProgramError {
    Conversion(parsing::TrussCreationError),
    Solving(solver::SolvingError),
    FileIO(String),
    Parsing(String),
}
fn use_new_parser(p: impl AsRef<std::path::Path>) -> Result<(), CommandLineProgramError> {
    let file = match std::fs::read_to_string(p) {
        Ok(s) => s,
        Err(e) => return Err(CommandLineProgramError::FileIO(e.to_string())),
    };
    let table = match file.parse::<toml::Table>() {
        Ok(t) => t,
        Err(e) => return Err(CommandLineProgramError::Parsing(e.to_string())),
    };
    let truss = match parsing::Truss2D::new(table) {
        Ok(t) => t,
        Err(e) => return Err(CommandLineProgramError::Conversion(e)),
    };
    let mut info = parsing::DebugInfo {
        enabled: true,
        output: Box::new(std::io::stderr()),
    };

    eprintln!("Names Conversion List:");
    for (id, name) in &truss.names {
        eprintln!("Item {id} has name: {name}");
    }
    eprintln!();

    let joints = truss.condense();
    eprintln!("Joints build from truss:");
    for j in &joints {
        eprintln!("{j}");
    }
    eprintln!();

    let solutions = match solver::solve_truss(&joints, &mut info) {
        Ok(v) => v,
        Err(e) => return Err(CommandLineProgramError::Solving(e)),
    };
    eprintln!();
    for solution in solutions {
        let name = match truss.names.get(&solution.force) {
            Some(n) => n.as_str(),
            None => "no name found"
        };
        eprintln!("Item {name} ({}) has value {:.8}", solution.force, solution.value);
    }
    Ok(())
}