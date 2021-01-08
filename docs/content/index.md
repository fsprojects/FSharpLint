---
title: Overview
category: top-level
menu_order: 1
---

# Overview

FSharpLint is a style checking tool for F#. It points out locations where a set of rules on how F# is to be styled have been broken.
The tool is configurable via JSON and can be run from a console app, or as an MSBuild task. It also provides an interface to easily integrate the tool into other software.

The project aims to let the user know of problems through [matching user defined hints](./how-tos/rules/FL0065.html)
a la [HLint](http://community.haskell.org/~ndm/hlint/), and also by using custom rules written in F# similar to the rules
in [Mascot](http://mascot.x9c.fr/manual.html) and [StyleCop](http://stylecop.codeplex.com/).

Using a `.fsproj` (F# project) or `.sln` (F# solution) file the tool will analyse all of the F# implementation files in the project/solution looking for
code that breaks a set of rules governing the style of the code. Examples of rules: lambda functions must be less than 6 lines long, class member identifiers must be PascalCase.

## Usage

FSharpLint can be used in several ways:

* [Running as dotnet tool from command line](./how-tos/install-dotnet-tool.html).
* [In VS Code using the Ionide-FSharp plugin](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp).
* [In other IDEs (Visual Studio, Rider) as an MSBuild Task](./how-tos/msbuild-task.html).
* [In other editors through FsAutoComplete Language Server](https://github.com/fsharp/FsAutoComplete)

### Example Usage

The following program:

    type ExampleInterface =
       abstract member print : unit -> unit

    [<EntryPoint>]
    let main argv =
        let x = List.fold (fun x y -> x + y) 0 [1;2;3]
        printfn "%d" x
        0

Run against the lint tool generates the following errors:

    FL0036: Consider changing `ExampleInterface` to be prefixed with `I`.
    Consider changing `ExampleInterface` to be prefixed with `I`.
    Error in file Program.fs on line 1 starting at column 5
    type ExampleInterface =
         ^

    FL0045: Consider changing `print` to PascalCase.
    Error in file Program.fs on line 2 starting at column 19
       abstract member print : unit -> unit
                       ^

    FL0034: If `( + )` has no mutable arguments partially applied then the lambda can be removed.
    Error in file Program.fs on line 6 starting at column 23
        let x = List.fold (fun x y -> x + y) 0 [1;2;3]
                           ^

Refactored using lint's warnings:

    type IExampleInterface =
       abstract member Print : unit -> unit

    [<EntryPoint>]
    let main argv =
        let x = List.fold (+) 0 [1;2;3]
        printfn "%d" x
        0

If we run lint again it will find a new error, it's worth running the tool until it no longer finds any errors:

    FL0065: `List.fold ( + ) 0 x` might be able to be refactored into `List.sum x`.
    Error in file Program.fs on line 6 starting at column 12
    let x = List.fold (+) 0 [1;2;3]
            ^

After refactoring again we have with no lint errors:

    type IExampleInterface =
       abstract member Print : unit -> unit

    [<EntryPoint>]
    let main argv =
        let x = List.sum [1;2;3]
        printfn "%d" x
        0

## Configuration Files

Configuration of the tool is done using JSON.
A single JSON file containing the default configuration for all rules
is [included inside of the software](https://github.com/fsprojects/FSharpLint/blob/master/src/FSharpLint.Core/DefaultConfiguration.json).

By default, FSharpLint will use the default configuration. You can override this to point
to a different file, for example by using the `--lint-config` flag in the dotnet tool.

See the [Rule Configuration page](./how-tos/rule-configuration.html) for more info.

## Suppressing rules in code

Rules can be disabled within the code using structured comments. See the [Suppressing Warnings](./how-tos/rule-suppression.html) page for more information.

## Running Lint From An Application

Install the [`FSharp.Core` nuget package](https://www.nuget.org/packages/FSharpLint.Core/).

The namespace `FSharpLint.Application` contains a module named `Lint` which provides several functions
to lint a project/source file/source string.
