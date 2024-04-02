Next:
- write macro to unwrap toml::Value values ?
- use Result error handling for validating points after they're parsed
- Should the parsing code remember the original point names for errors?
  - How would the errors be propagated so that that's useful?

To-do List (In no particular order):
1) Finalize how points are described
2) Design error system for the parser
3) Finish the format for problems in the TOML
4) Members from TOML
5) Loads in TOML
6) Supports in TOML
7) Invoke from commandline / pass target file at startup
8) Error type / design for the solver
9) Avoid using dependent equations in the matrix
10) Recover from a singular matrix
11) Ensure that each unknown appears in at least one equation
12) Validate input TOML files before parsing begins
13) get rid of the block comment in parsing.rs and put in the README (or other document)


In the future:
- Add relative positioning for points (maybe)
- Move panics + eprintln!() to the error type
- Good feedback / error messages for processing programs
- program startup / user experience
- recover from unnecessary info missing (e.g. nothing for weight defined) with defaults
- write a good README
- prep this to be an actual git repo