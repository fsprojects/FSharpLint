# FSharpLint [![GitHub Actions Build Status](https://github.com/fsprojects/FSharpLint/workflows/CI/badge.svg)](https://github.com/fsprojects/FSharpLint/actions?query=branch%3Amaster)

FSharpLint is a static analysis tool for F#. Like many other software linters, it analyzes your source code (via typed and untyped syntax trees), and then it points out locations where a set of rules on how F# is to be styled have been broken. There are various kinds of rules:

* Conventions: name your public or private API/ABI members to respect project or solution-wide conventions.
* Smells: get warned on logic that might pose maintainability problems or potential downsides.
* Formatting: indent and structure your code to follow a specific set of cosmetic standards.

The tool is configurable via JSON and can be run from a console app, or as an MSBuild task. It also provides an interface to easily integrate the tool into other software.

The project aims to let the user know of problems through [matching user defined hints](https://fsprojects.github.io/FSharpLint/how-tos/rules/FL0065.html) a la [HLint](http://community.haskell.org/~ndm/hlint/), and also by using custom rules written in F# similar to the rules in [Mascot](http://mascot.x9c.fr/manual.html) and [StyleCop](https://github.com/StyleCop/StyleCop).

The tool in use (running as an MSBuild task with TreatWarningsAsErrors set to true):

![Example](http://i.imgur.com/D4c9g1m.png)

## Usage

FSharpLint can be used in several ways:

* [Running as dotnet tool from command line](https://fsprojects.github.io/FSharpLint/how-tos/install-dotnet-tool.html).
* [In VS Code using the Ionide-FSharp plugin](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp).
* [In Visual Studio using the F# Lint extension](https://marketplace.visualstudio.com/items?itemName=asti.fslint-vs).
* [As an MSBuild Task](http://fsprojects.github.io/FSharpLint/how-tos/msbuild-task.html).
* [In other editors through FsAutoComplete Language Server](https://github.com/fsharp/FsAutoComplete)

## Documentation

The [docs](http://fsprojects.github.io/FSharpLint/) contain an overview of the tool and how to use it, including a list of the [available rules](http://fsprojects.github.io/FSharpLint/how-tos/rule-configuration.html#ruleList) for linting.

## Nuget Packages

Package | Version
------- | --------
[dotnet tool](https://www.nuget.org/packages/dotnet-fsharplint/) | [![NuGet Status](http://img.shields.io/nuget/v/dotnet-fsharplint.svg?style=flat)](https://www.nuget.org/packages/dotnet-fsharplint/)
[API](https://www.nuget.org/packages/FSharpLint.Core/) | [![NuGet Status](http://img.shields.io/nuget/v/FSharpLint.Core.svg?style=flat)](https://www.nuget.org/packages/FSharpLint.Core/)

## How to build

1. Make sure you've installed the .NET version defined in [global.json](global.json)
2. Run `dotnet tool restore` to install all developer tools required to build the project
3. Run `dotnet fsi build.fsx -t Build` to build (which executes the `Build` target from the FAKE-based [build script](build.fsx))
4. To run tests use `dotnet fsi build.fsx -t Test`
5. To build documentation use `dotnet fsi build.fsx -t Docs`

## How to work with documentation

1. Make sure you've installed the .NET version defined in [global.json](global.json)
2. Run `dotnet tool restore` to install all developer tools required to build the project
3. Run `dotnet fsi build.fsx` to build default target of [build script](build.fsx)
4. Build documentation to make sure everything is fine with `dotnet fsi build.fsx -t Docs`
5. Go to docs folder `cd docs` and start Fornax in watch mode `dotnet fornax watch`
6. Your documentation should be now accessible on `localhost:8080` and will be regenerated on every file save

## How to release

Please [read the Releasing Guidelines](./RELEASE.md) if you're a maintainer.

## How to contribute

Bug reports, feature requests, and pull requests are very welcome! Please [read the Contribution Guidelines](./CONTRIBUTING.md)
to get started.

## Licensing

The project is licensed under MIT. For more information on the license see the LICENSE file.

## Maintainer(s)

- [@duckmatt](https://github.com/duckmatt)
- [@knocte](https://github.com/knocte)
- [@jgardella](https://github.com/jgardella)

The default maintainer account for projects under "fsprojects" is [@fsprojectsgit](https://github.com/fsprojectsgit) - F# Community Project Incubation Space (repo management)
