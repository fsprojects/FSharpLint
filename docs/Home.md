#FSharpLint

FSharpLint is a lint tool for F#. It can be ran as a MSBuild task, or as a console application.

> The term [lint] is now applied generically to tools that flag suspicious usage in software written in any computer language - [_Wikipedia_](http://en.wikipedia.org/wiki/Lint_(software))

Using a `.fsproj` (F# project) file the tool will analyse all of the F# implementation files in a project looking for code that breaks a set of rules governing the style of the code. Examples of rules: lambda functions must be less than 6 lines long, class member identifiers must be PascalCase.

####Example Usage of the Tool

The following program:

    type ExampleInterface =
       abstract member print : unit -> unit

    [<EntryPoint>]
    let main argv = 
        let x = List.fold (fun x y -> x + y) 0 [1;2;3]
        printfn "%d" x
        0

Run against the lint tool generates the following errors:

    Interface identifiers should begin with the letter I found interface ExampleInterface
    Error in file Program.fs on line 1 starting at column 5
    type ExampleInterface =
         ^

    Expected pascal case identifier but was print
    Error in file Program.fs on line 2 starting at column 19
       abstract member print : unit -> unit
                       ^

    Pointless function redefines op_Addition
    Error in file Program.fs on line 6 starting at column 23
        let x = List.fold (fun x y -> x + y) 0 [1;2;3]
                           ^

After refactoring to the fix the lint errors, we end up with the following program:

    type IExampleInterface =
       abstract member Print : unit -> unit

    [<EntryPoint>]
    let main argv = 
        let x = List.fold (+) 0 [1;2;3]
        printfn "%d" x
        0

If we run lint again it will find a new error, it's worth running the tool until it no longer finds any errors:

    List.fold + 0 can be refactored into List.sum
    Error in file Program.fs on line 6 starting at column 12
    let x = List.fold (+) 0 [1;2;3]
            ^

After refactoring we end up with the following program with no lint errors:

    type IExampleInterface =
       abstract member Print : unit -> unit

    [<EntryPoint>]
    let main argv = 
        let x = List.sum [1;2;3]
        printfn "%d" x
        0

####Building The Tool

On windows run `build.cmd` and on unix based systems run `build.sh`. These will build the framework and put the console application into the `/bin` directory (the application will be named `fsharplint.exe`), all the tests will be run - all of which should pass. The MSBuild task assembly will have been built to `/src/FSharpLint.MSBuildIntegration/bin/Release/FSharpLint.MSBuildIntegration.dll`.

####Running The Tool

The tool can be run from the command line, or as an MSBuild task. 

* [Using the command line tool](Console-Application.md).
* [Using the MSBuild task](MSBuild-Task.md).

####Rules

| Analysers     |
| ------------- |
| FSharpLint.Hints |
| [FSharpLint.NameConventions](FSharpLint.NameConventions.md) |
| [FSharpLint.SourceLength](FSharpLint.SourceLength.md) |
| [FSharpLint.Typography](FSharpLint.Typography.md) |
| [FSharpLint.NestedStatements](FSharpLint.NestedStatements.md) |
| [FSharpLint.NumberOfItems](FSharpLint.NumberOfItems.md) |
| [FSharpLint.FunctionReimplementation](FSharpLint.FunctionReimplementation.md) |
| [FSharpLint.XmlDocumentation](FSharpLint.XmlDocumentation.md) |
| [FSharpLint.Binding](FSharpLint.Binding.md) |
| [FSharpLint.CyclomaticComplexity](FSharpLint.CyclomaticComplexity.md) |

Rules are grouped into sets of rules called analysers, the reason for this is that it allows for easy configuration of multiple related rules. For example turning off all xml documentation rules can be done by turning off just the analyser in the configuration.

####Configuration Files

Configuration of the tool is done using xml with a purposely similar structure to [StyleCop](http://stylecop.codeplex.com/). Configuration files must have the extension: `.FSharpLint`. A single xml file containing the default configuration for all rules is [included inside of the software](../src/FSharpLint.Framework/DefaultConfiguration.FSharpLint), these rules can then be overridden by creating new files in the directories of projects to be analysed (should be in the same directory as the .fsproj files). The rules are overridden by redefining any properties of an analyser/rule that you want to override, for example if you wanted to turn off the function reimplmentation analyser which has the default configuration of:

    <Analyser AnalyserId="FSharpLint.FunctionReimplementation">
      <Rules />
      <AnalyserSettings>
        <Property name="Enabled">True</Property>
      </AnalyserSettings>
    </Analyser>

To override to turn off you'd set enabled to false in your own configuration file as follows:

    <Analyser AnalyserId="FSharpLint.FunctionReimplementation">
      <Rules />
      <AnalyserSettings>
        <Property name="Enabled">False</Property>
      </AnalyserSettings>
    </Analyser>

####Running Lint From An Application

The project `src/FSharpLint.Application` contains `RunLint.fs` that contains the following function:

    /// <summary>Parses and runs the linter on all the files in a project.</summary>
    /// <param name="finishEarly">Function that when returns true cancels the parsing of the project, useful for cancellation tokens etc.</param>
    /// <param name="projectFile">Absolute path to the .fsproj file.</param>
    /// <param name="progress">Callback that's called at the start and end of parsing each file (or when a file fails to be parsed).</param>
    /// <param name="errorReceived">Callback that's called when a lint error is detected.</param>
    let parseProject (
        finishEarly: System.Func<bool>, 
        projectFile:string, 
        progress: System.Action<ParserProgress>, 
        errorReceived: System.Action<ErrorHandling.Error>)

This function is how all the current tools (MSBuild task and console application) run the lint tool on a single project. You pass it three functions: `finishEarly` lets you stop the lint tool at any time by returning true, `progress` gets passed information on what file is currently being linted within the project, every time a rule is found to be broken `errorReceived` is passed details on the broken rule.

If you've built the tool, the assembly to reference to access this function will be: `/bin/FSharpLint.Application.dll`