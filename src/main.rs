use itertools::Itertools;

use crate::display::display_truss_and_exit;
use crate::parsing::{Truss2D, TrussCreationError};
use crate::solver::{ComputedForce, SolvingError};

mod display;
mod parsing;
mod solver;
mod tests;

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
enum ForcedOutputMode {
    #[default]
    None,
    Quiet,
    Verbose,
}

/// Holds the different options for running force-system-solver.
#[derive(Debug, Default)]
struct CommandLineArguments {
    /// A bundled example problem for testing & showcasing things
    example: Option<&'static str>,
    problem_path: Option<std::path::PathBuf>,
    display_result: bool,
    /// Whether to ignore existing settings and not print any extra debug information
    silence_debug: ForcedOutputMode,
    graphics_debug: bool,
}
impl CommandLineArguments {
    fn get() -> Self {
        let mut options = CommandLineArguments::default();
        let args = std::env::args().collect_vec();

        if let Some(maybe_path) = args.get(1) {
            let path = std::path::PathBuf::from(maybe_path);
            if path.exists() {
                options.problem_path = Some(path);
            }
            // TODO: difference between whether the path was invalid or not intended to be one
        }

        // need to double-check arg 1 b/c it might not be a path
        for arg in args.iter().skip(1) {
            match arg.as_str() {
                "-e" | "-e=1" => {
                    options.example = Some(include_str!(r"..\sample-problems\problem-one.toml"))
                }
                "-e=2" => options.example = Some(include_str!(r"..\sample-problems\prob3.toml")),
                "-e=3" => options.example = Some(include_str!(r"..\sample-problems\prob4.toml")),
                "-g" => options.display_result = true,
                // TODO: what if user uses both -q and -v
                "--quiet" | "-q" => options.silence_debug = ForcedOutputMode::Quiet,
                "--verbose" | "-v" => options.silence_debug = ForcedOutputMode::Verbose,
                "-gdbg" => options.graphics_debug = true,
                "--help" | "-h" => {
                    eprintln!("Usage: force-system-solver <path to file> <options>");
                    eprintln!("Options:");
                    eprintln!("--help: show help, -e: run example problem, -g: display graphics");
                    eprintln!("--quiet: disable debug info, --verbose: enable debug info");
                },
                _ => {}
            }
        }
        options
    }
    fn is_valid(&self) -> Result<(), String> {
        if self.example.is_some() && self.problem_path.is_some() {
            return Err(String::from(
                "Warning: example requested while a path was provided!",
            ));
        }
        Ok(())
    }
    fn override_debug(&self, table: &mut toml::Table) {
        let output = match self.silence_debug {
            ForcedOutputMode::Verbose => toml::Value::Boolean(true),
            ForcedOutputMode::Quiet => toml::Value::Boolean(false),
            ForcedOutputMode::None => return,
        };
        table.insert(String::from("debug"), output);
    }

    /// Get a [toml::Table] for the truss to be solved. The returned table may come from the file
    /// system or a pre-written example problem. This function will ask the user to input a path
    /// if one is not otherwise specified.
    ///
    /// The returned Err() contains a message about what
    /// went wrong, e.g. issues finding or parsing the file.
    fn get_toml_table(&self) -> Result<toml::Table, String> {
        if let Some(example) = self.example {
            let mut table = example.parse::<toml::Table>().unwrap();
            self.override_debug(&mut table);
            return Ok(table);
        }
        if let Some(ref path) = self.problem_path {
            let file = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
            let mut table = file.parse::<toml::Table>().map_err(|e| e.to_string())?;
            self.override_debug(&mut table);
            return Ok(table);
        }
        let path = ask_user_for_path();
        if path.chars().all(|c| c.is_whitespace()) {
            return Err(String::from(
                "Blank path entered, use the flag '-e' to run an example!",
            ));
        }
        let file_text = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
        let mut table = file_text
            .parse::<toml::Table>()
            .map_err(|e| e.to_string())?;
        self.override_debug(&mut table);
        Ok(table)
    }
}

fn main() {
    let arguments = CommandLineArguments::get();
    if arguments.graphics_debug {
        eprintln!("Graphics Debug enabled, no other command args read.");
        eprintln!("Showing debugging truss");

        let truss = display::get_sample_truss();
        display_truss_and_exit(truss, Some("Graphics Sample"));
    }

    if let Err(message) = arguments.is_valid() {
        panic!("{}", message);
    }
    let toml_table = match arguments.get_toml_table() {
        Ok(t) => t,
        Err(message) => panic!("Could not find truss: {}", message),
    };

    let (truss, _) = solve_and_output_text(&toml_table);
    if arguments.display_result {
        let problem_info = parsing::ProblemInformation::get(&toml_table);
        display_truss_and_exit(truss, Some(&problem_info.name));
    }
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
    let mut p_info = parsing::ProblemInformation::get(table);
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
    let mut truss = match Truss2D::new(table) {
        Ok(t) => t,
        Err(TrussCreationError::PointNonExistent(m)) => panic!("Error: Non-existent point: {m}"),
        Err(TrussCreationError::PointsOverlap(m)) => panic!("Error: Points can not overlap: {m}"),
        Err(TrussCreationError::Conversion(c_e)) => panic!("Error: {c_e}"),
    };
    let joints = truss.condense();

    if out.enabled {
        let out_type = std::any::type_name_of_val(&*out.output);
        if out.display_names(&truss.names).is_err() {
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
            None => "No name found",
        };
        let state = if res.value > 0f64 { "T" } else { "C" };
        let id = res.force;

        match out.enabled {
            true => writeln!(out.output, "{name} [{id}]: {} ({state})", res.value),
            false => writeln!(out.output, "{name}: {:.8} ({state})", res.value),
        }
        .ok();
    }
    writeln!(out.output, "Solving complete!").ok();
    (truss, solutions)
}
