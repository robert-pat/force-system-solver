mod parsing;
mod solver;

/// Statics problem solver
fn main() {
    println!("Hello, World!");
}

fn ask_for_file() -> Option<String> {
    println!("Statics Truss Solver | ENGR 2120H");
    println!("Please enter a file name (relative to this directory):");

    let mut s = String::new();
    match std::io::stdin().read_line(&mut s) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Error in asking user for file at startup!");
            eprintln!("Tried to read from standard in: {e}");
            return None;
        }
    };

    let read_file = match std::fs::read_to_string(&s) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Error reading file \'{s}\' to being solving!");
            eprintln!("Saw error: {e}");
            return None;
        }
    };

    Some(read_file)
}
