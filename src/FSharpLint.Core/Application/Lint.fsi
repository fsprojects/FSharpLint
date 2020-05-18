namespace FSharpLint.Application

/// Provides an API to manage/load FSharpLint configuration files.
/// <see cref="FSharpLint.Framework.Configuration" /> for more information on
/// the default configuration and overriding configurations.
module ConfigurationManagement =

    open FSharpLint.Framework.Configuration

    /// Load a FSharpLint configuration file from the contents (string) of the file.
    val loadConfigurationFile : configurationFileText:string -> Configuration

/// Provides an API for running FSharpLint from within another application.
[<AutoOpen>]
module Lint =

    open System.Threading
    open FSharpLint.Core
    open FSharpLint.Framework
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.Rules
    open FSharp.Compiler
    open FSharp.Compiler.SourceCodeServices

    /// Provides information on what the linter is currently doing.
    [<NoComparison>]
    type LintProgress =
        /// Started parsing a file (file path).
        | Starting of string

        /// Individual lint warning received for a file.
        | ReceivedWarning of Suggestion.LintWarning

        /// Finished parsing a file (file path).
        | ReachedEnd of string * Suggestion.LintWarning list

        /// Failed to parse a file (file path, exception that caused failure).
        | Failed of string * System.Exception

    type ConfigurationParam =
        /// Explicit Configuration object to use.
        | Configuration of Configuration

        // Load configuration from provided file.
        | FromFile of configPath:string

        // Use default configuration.
        | Default

    /// Optional parameters that can be provided to the linter.
    [<NoEquality; NoComparison>]
    type LintParameters =
        { /// Cancels a lint in progress.
          CancellationToken: CancellationToken option

          /// Lint configuration to use.
          /// Can either specify a full configuration object, or a path to a file to load the configuration from.
          /// You can also explicitly specify the default configuration.
          Configuration: ConfigurationParam

          ReportLinterProgress: (LintProgress -> unit) option

          ReleaseConfiguration : string option }

        static member Default: LintParameters

    /// If your application has already parsed the F# source files using `FSharp.Compiler.Services`
    /// you want to lint then this can be used to provide the parsed information to prevent the
    /// linter from parsing the file again.
    [<NoEquality; NoComparison>]
    type ParsedFileInformation =
        { /// File represented as an AST.
          Ast: FSharp.Compiler.SyntaxTree.ParsedInput

          /// Contents of the file.
          Source: string

          /// Optional results of inferring the types on the AST (allows for a more accurate lint).
          TypeCheckResults: FSharp.Compiler.SourceCodeServices.FSharpCheckFileResults option

          /// Optional path to file for source.
          FilePath : string option }

    /// Reason for the linter failing.
    [<NoComparison>]
    type LintFailure =
        /// The specified file for linting could not be found.
        | FailedToLoadFile of string

        /// Failed to analyse a loaded FSharpLint configuration at runtime e.g. invalid hint.
        | RunTimeConfigError of string

        /// `FSharp.Compiler.Services` failed when trying to parse one or more files.
        | FailedToParseFiles of ParseFile.ParseFileFailure list

    /// Lints an entire F# solution by linting all projects specified in the `.sln` file.
    val lintSolution : optionalParams:LintParameters -> solutionFilePath:string -> Result<Suggestion.LintWarning list, LintFailure>

    /// Lints an entire F# project by retrieving the files from a given path to the `.fsproj` file.
    val lintProject : optionalParams:LintParameters -> projectFilePath:string -> Result<Suggestion.LintWarning list, LintFailure>

    /// Lints F# source code.
    val lintSource : optionalParams:LintParameters -> source:string -> Result<Suggestion.LintWarning list, LintFailure>

    /// Lints F# source code that has already been parsed using `FSharp.Compiler.Services` in the calling application.
    val lintParsedSource : optionalParams:LintParameters -> parsedFileInfo:ParsedFileInformation -> Result<Suggestion.LintWarning list, LintFailure>

    /// Lints an F# file from a given path to the `.fs` file.
    val lintFile : optionalParams:LintParameters -> filepath:string -> Result<Suggestion.LintWarning list, LintFailure>
