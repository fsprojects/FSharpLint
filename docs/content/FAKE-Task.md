# Running the FAKE Task

Install the [package via nuget](https://www.nuget.org/packages/FSharpLint.Fake/), the nuget package 
contains `FSharpLint.Fake.dll` which provides an `FSharpLint` function in the namespace `FSharpLint.Fake` which 
when called will run the task.

## Parameters

- `setParams ` - Function that can update the default options.
- `projectFile` - The project file of the project you want to lint

## Sample Usage

Run lint on multiple projects (here we are linting all projects with a directory in the `src` directory):

    #I @"packages/tools/FSharpLint.Fake/tools"
    #r @"packages/tools/FSharpLint.Fake/tools/FSharpLint.Fake.dll"
    open FSharpLint.Fake

    Target "Lint" (fun _ ->
        !! "src/**/*.fsproj"
            |> Seq.iter (FSharpLint id))

Run lint on a single project:

    #I @"packages/tools/FSharpLint.Fake/tools"
    #r @"packages/tools/FSharpLint.Fake/tools/FSharpLint.Fake.dll"
    open FSharpLint.Fake

    Target "Lint" (fun _ -> 
        FSharpLint id "src/FSharpLint.Application/FSharpLint.Application.fsproj")

## Setting Options

Use the identity function to keep the default options:

    FSharpLint id projectFile

For custom options, use a function that updates the options:

    FSharpLint (fun options -> { options with FailBuildIfAnyWarnings = true }) projectFile