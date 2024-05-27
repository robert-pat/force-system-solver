# ENGR 2120H: Honors Project
This is my submission for the Spring 2024 ENGR 2120H honors project: a Rust program to analyse a 2D truss 
and solve for the force each member is under. This project will read in a file (either opened in the program 
or passed in via the command line) and then attempt to solve the contained system.

## Solver Expectations & Solving Strategy:
The solver in this project uses the method of joints to create a series of linear equations from the
free-body diagrams of each joint in the truss. These equations are then converted into a matrix equation and solved. 
The program first constructs a sort of FBDs for each joint. These are then decomposed into linear equations, 2 per
joint. These equations are then packed into a matrix and solved.

## Creating a TOML file to solve a program
See ```template.toml``` for how to create a valid TOML file. I've tried to add useful error messages where possible, so
it's not too difficult to fix issues should they pop up. You can also look at one of the problems in ```sample-problems```.
The program does not support units, so make sure that everything (e.g. applied loads) are consistent. Also be sure
to check for typos from copy-pasting, if your truss has many points or external loads. 

#### Updates
This has already been submitted, so any future work is mostly as-I-feel. Right now I don't have any concrete plans
for what I'm wanting to add / improve.

#### Building & Running
Exactly what you'd expect for a rust project. Assuming you have rustup (see https://www.rust-lang.org/tools/install),
run ```cargo run```. File paths for inputs are relative. Absolute paths are handled by whatever ```std::fs::read_to_string()```
does with them. 

#### License
All rights reserved. Please do not use for training LLMs or generative AIs