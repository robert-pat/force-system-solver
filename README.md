# ENGR 2120H- Honors Project
This is my submission for the Spring 2024 ENGR 2120H honors project: a rust program to analyse a statics problem 
and provide solutions. This project will read in a file (either opened in the program or passed in via the command line)
and then attempt to solve the force system as best it can. This is expected to undergo many many revisions.

## Basic Outline / Theory of Operation
This project is in alpha & I will be changing these requirements a LOT. Right now here is what I'm thinking:
- A file opening / processing workflow similar to story-v2 w/ the ability to calculate a file directly from invoking the
executable and the ability to open & run the file from the terminal. 
- The force systems should be created & stored in a standard file format (TOML? JSON? XML?), but idk which
- There should be some way to indicate what the program is supposed to do
- The program should output a completed file of the same format w/ all of the information available (also print in console)


## Building / Running this project
When there is a working build, I will add it to the releases on the github sidebar. Untill then (or if you are on a different platform)
you will need to build from source:
1) Make sure rustup (the Rust toolchain) is installed. You can do this via the rust website: https://www.rust-lang.org/
2) Make sure you have a linker / build tools installed
    - On Windows: install the "Build Tools for Visual Studio" (NOT Visual Studio itself), follow directions on the rust website
    - MacOS: I think you need parts of the XCode toolchain (idk tho)
    - Linux: do whatever makes ```cargo build``` work
3) Once you have everything installed (check by running ```cargo build``` in a terminal), you can download the source code,
navigate into the folder and run ```cargo run``` to run the program.