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

	[lang=error]
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

	[lang=error]
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

#####Troubleshooting a failing build

On windows you may find yourself getting an error loading `FSharp.Data.DesignTime.dll` (HRESULT: 0x80131515), the solution to this is to right click on `lib/FSharp.Data.DesignTime.dll` in explorer, go to properties, and then click unblock.

Also on windows if the tests in `FSharpLint.Rules.Tests` are all failing with a `ParseException`, then the solution most probably is to dump `FSharp.Core.optdata` and `FSharp.Core.sigdata` into `tests\FSharpLint.Rules.Tests\bin\Release` or `tests\FSharpLint.Rules.Tests\bin\Debug` depending upon your build target. This shouldn't be required, it seems to only happen on some builds on some computers. To get the `FSharp.Core.optdata` and `FSharp.Core.sigdata` you need to find where your FSharp.Core is located - the path will be something along the lines of `C:\Program Files\Reference Assemblies\Microsoft\FSharp\3.0\Runtime\v4.0` (F# 3.0) or `C:\Program Files\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.1.0` (F# 3.1)

####Running The Tool

The tool can be run from the command line, or as an MSBuild task. 

* [Using the command line tool](Console-Application.html).
* [Using the MSBuild task](MSBuild-Task.html).
* [Using the FAKE task](FAKE-Task.html).

####Analysers

* [Hints](Hints.html)
* [NameConventions](NameConventions.html)
* [SourceLength](SourceLength.html)
* [Typography](Typography.html)
* [NestedStatements](NestedStatements.html)
* [NumberOfItems](NumberOfItems.html)
* [FunctionReimplementation](FunctionReimplementation.html)
* [XmlDocumentation](XmlDocumentation.html)
* [Binding](Binding.html)
* [RaiseWithTooManyArguments](RaiseWithTooManyArguments.html)

Rules are grouped into sets of rules called analysers, the reason for this is that it allows for easy configuration of multiple related rules. For example turning off all xml documentation rules can be done by turning off just the analyser in the configuration.

####Configuration Files

Configuration of the tool is done using xml with a purposely similar structure to [StyleCop](http://stylecop.codeplex.com/). Configuration files must be named: `Settings.FSharpLint`. A single xml file containing the default configuration for all rules is [included inside of the software](https://github.com/fsprojects/FSharpLint/blob/master/src/FSharpLint.Framework/DefaultConfiguration.FSharpLint).

The configuration files are loaded in a specific order, files loaded after another will override the previous file. The default configuration is loaded first, from there the tool checks each directory from the root to up to the project being linted's directory. For example if the path of the project being linted was `C:\Files\SomeProjectBeingLinted`, then `C:\` would first be checked for a config file - if a config file is found then it would override the default config, then `C:\Files` would be checked - if a config file was found and a config file was also previously found in `C:\` then the config in `C:\Files` would override the one in `C:\` and so on.

The configuration rules are overridden by redefining any properties of an analyser/rule that you want to override, for example if you wanted to turn off the function reimplmentation analyser which has the default configuration of:

	[lang=xml]
    <FunctionReimplementation>
      <Enabled>True</Enabled>
    </FunctionReimplementation>

To override to turn off you'd set enabled to false in your own configuration file as follows:

	[lang=xml]
    <?xml version="1.0" encoding="utf-8"?>
	<FSharpLintSettings>
	    <Analysers>
			<FunctionReimplementation>
				<Enabled>True</Enabled>
			</FunctionReimplementation>
		</Analysers>
	</FSharpLintSettings>

####Disabling rules and analysers within code

To disable a rule for a single section of code the [SuppressMessageAttribute](http://msdn.microsoft.com/en-us/library/system.diagnostics.codeanalysis.suppressmessageattribute(v=vs.110).aspx) can be used. The attribute will not be included into the IL (as long as you don't have the `CODE_ANALYSIS` preprocessing symbol set).

The attribute can be applied to let bindings, modules, types, and exceptions. Any lint warnings generated on or inside of what the attribute is applied to will be suppressed.

#####Category and CheckId

Only two properties in the attribute need to be set to suppress a rule, these are: `Category` and `CheckId`. The attribute has a constructor which takes these two as the arguments, and they can also be set through property initialisation in the default constructor.

Category is the name of the analyser containing the rule to be suppressed e.g. `NameConventions`.

CheckId is the name of the rule to be suppressed, or `*` to suppress all rules in the analyser e.g. `InterfaceNamesMustBeginWithI`.

#####Example

The following code breaks the rule `NameConventions.InterfaceNamesMustBeginWithI`:

    type Printable =
        abstract member Print : unit -> unit
        
It will result in the warning: `Interface identifiers should begin with the letter I found interface Printable`.

We can disable this rule with: `[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "InterfaceNamesMustBeginWithI")>]`

The following code has the `SuppressMessage` attribute applied to it, this code will generate no warning:

    [<SuppressMessage("NameConventions", "InterfaceNamesMustBeginWithI")>]
    type Printable =
        abstract member Print : unit -> unit
        
        
This could also be written using property initialisers as:

    [<SuppressMessage(Category = "NameConventions", CheckId = "InterfaceNamesMustBeginWithI")>]
    type Printable =
        abstract member Print : unit -> unit

####Ignoring Files

In the configuration file paths can be used to specify files that should be included, globs are used to match wildcard directories and files. For example the following will match all files with the file name assemblyinfo (the matching is case insensitive) with any extension:

	<IgnoreFiles Update="Add">
		<![CDATA[
		  assemblyinfo.*
		]]>
	</IgnoreFiles>

* The `Update` attribute can be set to `Add` or `Overwrite` (if the attribute does not exist it will default to `Overwrite`). `Add` will add the paths to existing paths, `Overwrite` will only use the paths you have specified.
* The paths must be separated by new lines.
* Directories in the path must be separated using `/`
* If the path ends with a `/` then everything inside of a matching directory shall be excluded.
* If the path does not end with a `/` then all matching files are excluded.

####Running Lint From An Application

The project `src/FSharpLint.Application` contains `RunLint.fs` that contains the following function:

    /// Parses and runs the linter on all the files in a project.
    val parseProject : projectInformation: ProjectParseInfo -> Result

This function is how all the current tools (MSBuild task and console application) run the lint tool on a single project. You pass it a record with information needed to locate the project along with three functions: `finishEarly` lets you stop the lint tool at any time by returning true, `progress` gets passed information on what file is currently being linted within the project, every time a rule is found to be broken `errorReceived` is passed details on the broken rule.

If you've built the tool, the assembly to reference to access this function will be: `/bin/FSharpLint.Application.dll`