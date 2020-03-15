# FSharpLint

FSharpLint is a style checking tool for F#. It points out locations where a set of rules on how F# is to be styled have been broken.
The tool is configurable via JSON and can be run from a console app, or as an MSBuild task. It also provides an interface to easily integrate the tool into other software.

The project aims to let the user know of problems through [matching user defined hints](http://fsprojects.github.io/FSharpLint/rules/FL0065.html) a la [HLint](http://community.haskell.org/~ndm/hlint/), and also by using custom rules written in F# similar to the rules in [Mascot](http://mascot.x9c.fr/manual.html) and [StyleCop](http://stylecop.codeplex.com/).

The tool in use (running as an MSBuild task with TreatWarningsAsErrors set to true):

![Example](http://i.imgur.com/D4c9g1m.png)

## Usage

FSharpLint can be used in several ways:

* [Running as dotnet tool from command line](http://fsprojects.github.io/FSharpLint/DotnetTool.html).
* [In VS Code using the Ionide-FSharp plugin](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp).
* [In other IDEs (Visual Studio, Rider) as an MSBuild Task](http://fsprojects.github.io/FSharpLint/MSBuildTask.html).
* [In other editors through FsAutoComplete Language Server](https://github.com/fsharp/FsAutoComplete)

## Documentation

The [docs](http://fsprojects.github.io/FSharpLint/) contains an overview of the tool and how to use it, including a list of the [available rules](http://fsprojects.github.io/FSharpLint/Rules.html) for linting.

## Nuget Packages

Package | Version
------- | --------
[dotnet tool](https://www.nuget.org/packages/dotnet-fsharplint/) | [![NuGet Status](http://img.shields.io/nuget/v/dotnet-fsharplint.svg?style=flat)](https://www.nuget.org/packages/dotnet-fsharplint/)
[API](https://www.nuget.org/packages/FSharpLint.Core/) | [![NuGet Status](http://img.shields.io/nuget/v/FSharpLint.Core.svg?style=flat)](https://www.nuget.org/packages/FSharpLint.Core/)

## Build Status

Mono 4.0.2 | .NET 4.5
---------- | --------
[![Mono CI Build Status](https://travis-ci.org/fsprojects/FSharpLint.svg?branch=master "Build Status")](https://travis-ci.org/fsprojects/FSharpLint) | [![.NET CI Build status](https://ci.appveyor.com/api/projects/status/l4d22kby012cb7jf "Build Status")](https://ci.appveyor.com/project/duckmatt/fsharplint-231)

## Licensing

The project is licensed under MIT. For more information on the license see the LICENSE file.

## Contact

Feel free to post an issue on [github](../../issues) if you have any questions, have suggestions, or have found a defect.

## Maintainer(s)

- [@duckmatt](https://github.com/duckmatt)

The default maintainer account for projects under "fsprojects" is [@fsprojectsgit](https://github.com/fsprojectsgit) - F# Community Project Incubation Space (repo management)
