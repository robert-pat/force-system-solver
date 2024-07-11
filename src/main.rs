use crate::display::display_truss_and_exit;
use crate::parsing::{ParsingError, Truss2D, TrussCreationError};
use crate::solver::{ComputedForce, SolvingError};
use std::collections::HashSet;
use toml::Value;

mod display;
mod parsing;
mod solver;
mod tests;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum CommandFlags {
    OldParser,
    UseExample,
    DisplayWhenDone,
    ForceNoDebug,
    GraphicsDebug,
}
fn main() {
    let mut ex_number: Option<i32> = None;
    let mut flags: HashSet<CommandFlags> = HashSet::new();
    for arg in std::env::args() {
        match arg.as_str() {
            "--old" => flags.insert(CommandFlags::OldParser),
            "-e" => flags.insert(CommandFlags::UseExample),
            "-e=1" => {
                ex_number = Some(1);
                flags.insert(CommandFlags::UseExample)
            }
            "-e=2" => {
                ex_number = Some(2);
                flags.insert(CommandFlags::UseExample)
            }
            "-e=3" => {
                ex_number = Some(3);
                flags.insert(CommandFlags::UseExample)
            }
            "-g" => flags.insert(CommandFlags::DisplayWhenDone),
            "--quiet" => flags.insert(CommandFlags::ForceNoDebug),
            "-gdbg" => flags.insert(CommandFlags::GraphicsDebug),
            _ => true,
        };
    }

    // Graphics debugging case, not really intended for anything else;
    if flags.contains(&CommandFlags::GraphicsDebug) {
        eprintln!("Graphics Debug enabled, no other command args read.");
        eprintln!("Showing debugging truss");
        drop(flags);

        let truss = display::get_sample_truss();
        display_truss_and_exit(truss);
    }

    // Case of asking for the example problem, regardless of other settings
    let mut toml_table = if flags.contains(&CommandFlags::UseExample) {
        let text = match ex_number {
            Some(2) => include_str!(r"..\sample-problems\prob3.toml"),
            Some(3) => include_str!(r"..\sample-problems\prob4.toml"),
            _ => include_str!(r"..\sample-problems\problem-one.toml"),
        };
        text.parse::<toml::Table>().unwrap()
    } else {
        let path = ask_user_for_path();
        if path.chars().all(|c| c.is_whitespace()) {
            eprintln!("Blank path entered, use the flag '-e' to run an example!");
            eprintln!("Exiting ...");
            return;
        }
        #[allow(unused_variables)]
        let file_text = match std::fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => panic!("could not open the specified file: {e:?}"),
        };
        #[allow(unused_variables)]
        match file_text.parse::<toml::Table>() {
            Ok(t) => t,
            Err(e) => panic!("did not find a valid toml file: {e:?}"),
        }
    };

    if flags.contains(&CommandFlags::ForceNoDebug) {
        if let Some(Value::Boolean(b)) = toml_table.get_mut("debug") {
            *b = false
        };
    }

    // work around the old parser (till its removed...)
    if flags.contains(&CommandFlags::OldParser) {
        let path = if flags.contains(&CommandFlags::UseExample) {
            eprintln!(
                r"Using a sample problem! Old parser expects the example at sample-problems\problem-one.toml"
            );
            Some(r"sample-problems\problem-one.toml".to_string())
        } else {
            None
        };
        legacy_parser(path);
        return;
    }

    let (truss, _) = solve_and_output_text(&toml_table);
    if flags.contains(&CommandFlags::DisplayWhenDone) {
        display_truss_and_exit(truss);
    }
}

/// Statics problem solver! Solving 2D Trusses in Static Equilibrium
fn legacy_parser(path: Option<String>) {
    // get the file we need, skip the program name (first arg)
    let file_path = path.unwrap_or(ask_user_for_path());
    let file = match std::fs::read_to_string(&file_path) {
        Ok(f) => f,
        Err(e) => panic!("Error opening file: {:?}", e),
    };

    // startup messages
    let table = file.parse::<toml::Table>().unwrap();
    let mut info = parsing::get_problem_information(&table);
    println!("..........");
    println!(
        "Statics Problem Solver | Solving {} at \'{file_path}\'",
        info.name
    );
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

/// Uses the (already created) [toml::Table] as input for the (new) parser and solver.
/// This is handle writing text output to the appropriate location (specified in the input file) and
/// formating for errors encountered in the solving process.
///
/// This functions returns the parsed truss and calculated values, but these can be ignored.
fn solve_and_output_text(table: &toml::Table) -> (Truss2D, Vec<ComputedForce>) {
    let mut p_info = parsing::get_problem_information(table);
    let out = &mut p_info.debug;
    writeln!(
        out.output,
        "Statics Problem Solver | Working on {}",
        p_info.name
    )
    .ok();
    if out.enabled {
        eprintln!("-> Debug information is enabled; extra info will be output <-");
    }
    if p_info.file_write {
        eprintln!(
            "-> File writing is enabled! Output & answers written to answer-{}.txt <-",
            p_info.name
        );
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
        Err(SolvingError::NoMatrixWorked) => panic!("No invertible matrix found for this problem"),
    };
    for res in &solutions {
        let name = match truss.names.get(&res.force) {
            Some(n) => n.as_str(),
            None => "no name found",
        };
        let state = if res.value > 0f64 { "T" } else { "C" };
        let id = res.force;

        match out.enabled {
            true => writeln!(out.output, "{name} [{id}]: {} ({state})", res.force),
            false => writeln!(out.output, "{name}: {} ({state})", res.force),
        }
        .ok();
    }
    writeln!(out.output, "Solving complete!").ok();
    (truss, solutions)
}
