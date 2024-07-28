# ENGR 2120H: Honors Project
This was my submission for the Spring 2024 ENGR 2120H honors project: a Rust program to analyse a 2D truss 
and solve for the force each member is under. In the time since submitting, I've worked on the project a 
fair ammount, and the current version is somewhat different than the original submission. Please see the 
tagged commits for the versions that I originally submitted.

## Solver Expectations & Solving Strategy:
This project uses the method of joints to solve a two-dimensional truss in static equilibrium. Conceptually it creates 
a series of linear equations from the free-body diagrams of each joint in the truss. These equations are then combined into 
matrix form and solved with linear algebra.

These 'free-body diagrams' are represented in the TrussJoint2D structs, which are created in the ```Truss2D::condense()```
method. These are turned into equations (and ultimately a matrix equation) in the ```build_equations()``` and ```solve_truss``` 
functions (found in ```solver.rs```).

## Creating a TOML file to solve a program
See ```template.toml``` for how to create a valid TOML file. I've tried to add useful error messages where possible, so
it's hopefully not too difficult to fix issues if they pop up. You can also look in ```sample-problems``` for some examples.
A few things to keep in mind:
 - The program does not support units, so make sure that everything (e.g. applied loads, distances) are consistent. 
 - Angles are all measured in degrees.
 - There are currently no checks to make sure a truss is actually in equilibrium, answers will be wrong if it's not.
 - Check for typos and duplicate points / loads from copy-pasting, especially if you use an example file.

#### Updates
This has long since been submitted and graded, so I'm mostly adding things as I feel like and/or get bored. I don't
really have any major plans for what this program will be, so yeah. So far I've added:
 - Visualizing a truss 
 - Command line flags for different behavior
 - Rewrote the code to parse a truss (w/ arguably no difference in functionality)

#### Building & Running
Exactly what you'd expect for a rust project. Assuming you have [rustup](https://www.rust-lang.org/tools/install),
run with ```cargo run```. File paths for inputs are relative to the executable. It might take a sec for the dependencies
to download / compile the first time you run it 
(Sorry, linear algebra libraries are hard to make & drawing to the screen is worse).

#### License
All rights reserved. Please do not use for training LLMs or generative AIs
