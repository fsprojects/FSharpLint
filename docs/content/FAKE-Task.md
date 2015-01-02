#Using the FAKE Task

The [FAKE](http://fsharp.github.io/FAKE/) task can be accessed by referencing `FSharpLint.FAKE.dll`, this dll is built to `src/FSharpLint.FAKE/bin/Release/FSharpLint.FAKE.dll` and also included in the [nuget package](https://www.nuget.org/packages/FSharpLint/).

## Parameters

- `setParams ` - Function that can update the default options.
- `projectFile` - The project file of the project you want to lint

## Sample Usage

Run lint on multiple projects (here we are linting all projects with a directory in the `src` directory):

    #r @"packages/FSharpLint.0.1.12/FSharpLint.FAKE.dll"
    open FSharpLint.FAKE

    Target "Lint" (fun _ ->
        !! "src/**/*.fsproj"
            |> Seq.iter (FSharpLint id))

Run lint on a single project:

    #r @"packages/FSharpLint.0.1.12/FSharpLint.FAKE.dll"
    open FSharpLint.FAKE

    Target "Lint" (fun _ -> 
        FSharpLint id "src/FSharpLint.Application/FSharpLint.Application.fsproj")

## Setting Options

Use the identity function to keep the default options:

    FSharpLint id projectFile

Use a function that updates the options record to change the default options (here we are setting a directory for FSharpLint to lookup `FSharp.Core.dll` from):

    FSharpLint (fun options -> { options with FSharpCoreDirectory = "/somedirectory" }) projectFile