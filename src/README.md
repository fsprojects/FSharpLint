#FSharpLint/src

##Directories

###FSharpLint.Application

A class library project provides a simple interface to run the lint tool on code from within a program, right now the console app and visual studio extension included in this solution go through this.

The main work of this project is analysing .fsproj files to get the list of F# files, get the list of references and resolve those references to their absolute paths.

###FSharpLint.Console

A console app that right now isn't doing a whole lot other than running some code in a literal string (for debugging purposes). Eventually the idea is for this to be an app that will lint F# projects (can easily be set up right now to do this) without the use of a visual studio extension.

###FSharpLint.Framework

Contains the core functionality of the solution, this includes walking an abstract syntax tree, general syntax tree functions, reading and overriding config files.

###FSharpLint.Rules

Each file should contain a set of related lint rules e.g. checking code follows standard naming conventions. The rules are implemented using a visitor function which is then applied to each node in an abstract syntax tree, the walking of the tree can be controlled through the return value of the visitor function.
