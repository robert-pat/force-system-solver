use std::collections::HashSet;
use std::path::Path;
use crate::parsing::{ParsingError, Truss2D, TrussCreationError};
use crate::solver::SolvingError;

mod parsing;
mod solver;
mod tests;
#[path = r"graphics\display.rs"]
mod display;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum CommandFlags {
    NewParser,
    UseExample,
    UseRenderer,
}
fn main () {
    let mut flags: HashSet<CommandFlags> = HashSet::new();
    if std::env::args().any(|a| a.as_str() == "-n") {
        flags.insert(CommandFlags::NewParser);
    }
    if std::env::args().any(|a| a.as_str() == "-g") {
        flags.insert(CommandFlags::UseRenderer);
        display::init(Truss2D::default());
        return;
    }
    if std::env::args().any(|a| a.as_str() == "-e") {
        flags.insert(CommandFlags::UseExample);
        eprintln!("Examples automatically run with the new parser.");
        main_new_parser(Some(String::new()));
        return;
    }

    if flags.contains(&CommandFlags::NewParser) {
        let mut path = std::env::args().skip(1).filter(|a| Path::new(a).exists());
        main_new_parser(path.next());
        return;
    }
    legacy_parser();
}

/// Statics problem solver! Solving 2D Trusses in Static Equilibrium
fn legacy_parser() {
    // get the file we need, skip the program name (first arg)
    let file_path = std::env::args().nth(2).unwrap_or_else(ask_user_for_path);
    let file = match std::fs::read_to_string(&file_path) {
        Ok(f) => f,
        Err(e) => panic!("Error opening file: {:?}", e),
    };

    // startup messages
    let table = file.parse::<toml::Table>().unwrap();
    let mut info = parsing::get_problem_information(&table);
    println!("..........");
    println!("Statics Problem Solver | Solving {} at \'{file_path}\'", info.name);
    if info.debug.enabled {
        eprintln!("------------");
        eprintln!("Debug info enabled! The parser & solver will spit out a lot of text!");
        eprintln!("------------");
    }
    if info.file_write {
        eprintln!("File writing is enabled! Output (debug & results) will be printed to \'answer-{}.txt\'", info.name);
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
            writeln!(info.debug.output, "Id {id} is \'{name}\'").unwrap();
        }

        writeln!(info.debug.output, "Joints:").unwrap();
        for joint in &joints {
            writeln!(info.debug.output, "{joint}").unwrap(); // lists the forces acting at the joint
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
/// If no path is specified, the default one will be used instead.
fn ask_user_for_path() -> String {
    println!("Please enter a file path to the problem you would like solved;");
    println!("this path should be relative to the current directory.");
    let mut s = String::new();
    std::io::stdin()
        .read_line(&mut s)
        .expect("Could not read input (from stdin), this is likely a program error");

    s.trim().to_string() // I know, I know
}

/// Run the program to use the new parser (converting to a [Truss2D] first). The outputs
/// are equally accurate as the old parser, but formatting may slightly differ. Using the new
/// parser also produced more information about the Truss (see Truss2D).
fn main_new_parser(problem_path: Option<String>) {
    let problem_path = problem_path.unwrap_or_else(ask_user_for_path);
    if problem_path.chars().all(|c| c.is_whitespace()) {
        eprintln!("Empty path detected! Here's a sample problem:");
        let file_text = include_str!(r"..\sample-problems\problem-one.toml");
        let table = match file_text.parse::<toml::Table>() {
            Ok(t) => t,
            Err(e) => panic!("did not find a valid toml file: {}", e),
        };
        run_program(&table);
        return;
    }

    let table = {
        let file_text = match std::fs::read_to_string(&problem_path) {
            Ok(t) => t,
            Err(e) => panic!("could not read the file: {}", e),
        };
        match file_text.parse::<toml::Table>() {
            Ok(t) => t,
            Err(e) => panic!("did not find a valid toml file: {}", e),
        }
    };
    run_program(&table);
}
/// Uses the (already created) [toml::Table] as input for the (new) parser and solver. This function
/// is a full 'main' for force-system-solver, i.e. it responds to debug output settings, file
/// writing, and everything else. See [main_new_parser()] for a 'main' function that also handles
/// everything from file path to creating the Table.
fn run_program(table: &toml::Table) {
    let mut p_info = parsing::get_problem_information(table);
    let out = &mut p_info.debug;
    writeln!(out.output, "Statics Problem Solver | Working on {}", p_info.name).ok();
    if out.enabled {
        eprintln!("-> Debug information is enabled; extra info will be output <-");
    }
    if p_info.file_write {
        eprintln!("-> File writing is enabled! Output & answers written to answer-{}.txt <-", p_info.name);
    }
    #[allow(unused)] // IntelliJ can't read format strings for use
    let truss = match Truss2D::new(table) {
        Ok(t) => t,
        Err(TrussCreationError::PointNonExistent(m)) => panic!("Error: Non-existent point: {m}"),
        Err(TrussCreationError::PointsOverlap(m)) => panic!("Error: Points can not overlap: {m}"),
        Err(TrussCreationError::Conversion(c_e)) => panic!("Error: {c_e}"),
    };
    let joints = truss.condense();

    if out.enabled {
        let out_type = std::any::type_name_of_val(&*out.output);
        if out.display_names(truss.names.iter()).is_err() {
            eprintln!("Warning: issue writing name information to {out_type:?}");
        }
        if out.display_joints(&joints).is_err() {
            eprintln!("Warning: issue writing name information to {out_type:?}");
        }
    }

    let solutions = match solver::solve_truss(&joints, out) {
        Ok(r) => r,
        Err(SolvingError::NoMatrixWorked) =>  panic!("No invertible matrix found for this problem"),
    };
    for res in solutions {
        let name = match truss.names.get(&res.force) {
            Some(n) => n.as_str(),
            None => "no name found",
        };
        let state = if res.value > 0f64 { "T" } else { "C" };
        let id = res.force;

        match out.enabled {
            true => writeln!(out.output, "{name} [{id}]: {} ({state})", res.force),
            false => writeln!(out.output, "{name}: {} ({state})", res.force),
        }.ok();
    }
    writeln!(out.output, "Solving complete. Exiting.").ok();
}