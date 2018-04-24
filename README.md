# cs225-Final
final project for 225

To build the project code, and look at the test cases the only command that needs
to be run is make "filename", all the test cases are prewritten and will display any
errors that may arise from faulty code, so a third party could, write their own test 
cases at the bottom of the .ml file provided.

Make sure you have ocaml 4.06.0, and ppx_deriving installed.

    > opam switch 4.06.0
    > opam install ppx_deriving

To build our project and run the ocaml code, you need a .merlin file and a makefile
to help merlin and ocamlbuild understand how to build the project.

File 1) Hidden .merlin :
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
