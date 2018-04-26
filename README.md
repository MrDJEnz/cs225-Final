# cs225-Final
final project for 225

To build the project code, and look at the test cases the only command that needs
to be run is make "filename", all the test cases are prewritten and will display any
errors that may arise from faulty code, so a third party could, write their own test 
cases at the bottom of the .ml file provided.

The project is found in the checkpoint 2 folder, and the program that will be run is an 
extension of the OCaml language, based off of the book "Advanced Topics in Types and 
Programming Languages -- Chapter 3" involving stack allocation memory lifetimes. We will design 
functions which will be allowed to allocate memory on its stack for a value, and then pass
a reference to that value to functions called within the body of the function. The point of 
introducing effect types and regions into OCaml code allows us to use it as an application
domain to develop fundamental concepts of effect type systems step by step.

Make sure you have ocaml 4.06.0, and ppx_deriving installed.

    > opam switch 4.06.0
    > opam install ppx_deriving

To build our project and run the ocaml code, you need a .merlin file and a makefile
to help merlin and ocamlbuild understand how to build the project.

File 1) Hidden .merlin:

    > this finds all source files in the directory
    > find the build files in the _build directory
    > uses ppx_deriving.std package while checking the files
    
File 2) _tags:

    > This file instructs ocamlbuild to use the ppx_deriving.std package when compiling files.

    
There is an included Makefile that will build all *.ml files in the current
directory as the default target:

    > make
  
To execute a file, "filename".ml you run the command:
    
    >  make "filename"
    
Running make, or make "filename", creates compiled files, placed into 
a _build directory, (this will be automatically created if not already existing.
to get rid of the built directory file run the command:

    >  make clean
