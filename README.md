# ENGR 2120H: Honors Project
This is my submission for the Spring 2024 ENGR 2120H honors project: a Rust program to analyse a 2D truss 
and solve for the force each member is under. This project will read in a file (either opened in the program 
or passed in via the command line) and then attempt to solve the force system as best it can. This is 
expected to undergo many revisions.

## Basic Outline / Theory of Operation
This project is in alpha & I will be changing these requirements a LOT. Right now here is what I'm thinking:
- See ```parsing.rs``` for the current general plan
- TODO: write up the assumptions & invariants here

### Research on other programs / structure:
- Def need to use Matrices--could use my WIP library, but I'd rather just use an existing proven one
- 
### Solver Expectations & Solving Strategy:
The solver in this project mainly works by using the Method of Joints to create a series of linear equations from the
free-body diagrams of each joint in the truss. These equations are then combined into a matrix which is solved using matrix
algebra (matrix multiplication). The solver also performs some optimizations before the full matrix is constructed.

## Building / Running this project
When there is a working build, I will add it to the releases on the GitHub sidebar. If you are on an x86-64 Windows device, you can download and run the executable. Until then (or if you are on a different platform) you will need to build from source:
1) Make sure ```rustup``` (the Rust toolchain) is installed. You can do this via the rust website: https://www.rust-lang.org/
2) Run ```rustup update``` (unless you freshly installed it) to get the latest version of Rust
3) Make sure you have a linker / build tools installed
    - On Windows: install the "Build Tools for Visual Studio" (NOT Visual Studio itself), follow directions on the rust website
    - MacOS: I think you need parts of the XCode toolchain (IDK tho)
    - Linux: do whatever makes ```cargo build``` work; you're on your own
4) Once you have everything installed (check by running ```cargo build``` in a terminal), you can download the source code,
navigate into the folder and run ```cargo run``` to run the program.

### Creating a TOML file to solve a program
