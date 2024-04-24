# ENGR 2120H: Honors Project
This is my submission for the Spring 2024 ENGR 2120H honors project: a Rust program to analyse a 2D truss 
and solve for the force each member is under. This project will read in a file (either opened in the program 
or passed in via the command line) and then attempt to solve the force system as best it can.

## Solver Expectations & Solving Strategy:
The solver in this project mainly works by using the Method of Joints to create a series of linear equations from the
free-body diagrams of each joint in the truss. These equations are then converted into a matrix equation and solved. 
I plan to add more detail here and in the code about usage and how everything works in the future.

## Creating a TOML file to solve a program
See ```template.toml``` for how to create a valid TOML file. I've tried to add useful error messages where possible, so
it's not too difficult to fix issues should they pop up. You can also look at one of the problems in ```sample-problems```.

#### License
All rights reserved. Please do not use for training LLMs or generative AIs