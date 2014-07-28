#FSharpLint/src

##Directories

###FSharpLint.Application

A library project provides a simple interface to run the lint tool on code from within a program, right now the console app and visual studio extension include

The main work of this project is analysing .fsproj files to get the list of F# files, get the list of references and resolve those references to their absolute

###FSharpLint.Console

A console app that runs the linter against an F# project, taking the .fsproj file location as an argument and outputting any lint errors found.

###FSharpLint.Framework

Contains the core functionality of the solution, this includes walking an abstract syntax tree, general syntax tree functions, parsing hints, reading and overriding config files.

###FSharpLint.MSBuildIntegration

A custom MSBuild task that runs lint against an F# project and outputs any lint errors as build warnings.

###FSharpLint.Rules

Each file in this directory contains set of related lint rules e.g. checking code follows standard naming conventions. The rules are implemented using a visitor function which is then applied to each node in an abstract syntax tree, the walking of the tree can be controlled through the return value of the visitor function.