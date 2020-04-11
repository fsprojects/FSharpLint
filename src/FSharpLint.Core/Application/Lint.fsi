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
    type ProjectProgress =
        /// Started parsing a file (file path).
        | Starting of string

        /// Finished parsing a file (file path).
        | ReachedEnd of string * Suggestion.LintWarning list

        /// Failed to parse a file (file path, exception that caused failure).
        | Failed of string * System.Exception

        /// Path of the F# file the progress information is for.
        member FilePath : unit -> string

    type ConfigurationParam =
        | Configuration of Configuration
        | FromFile of configPath:string
        | Default

    /// Optional parameters that can be provided to the linter.
    [<NoEquality; NoComparison>]
    type OptionalLintParameters =
        { /// Cancels a lint in progress.
          CancellationToken: CancellationToken option

          /// Lint configuration to use.
          /// Can either specify a full configuration object, or a path to a file to load the configuration from.
          /// You can also explicitly specify the default configuration.
          Configuration: ConfigurationParam

          /// This function will be called every time the linter finds a broken rule.
          ReceivedWarning: (Suggestion.LintWarning -> unit) option

          ReportLinterProgress: (ProjectProgress -> unit) option

          ReleaseConfiguration : string option }

        static member Default: OptionalLintParameters

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
          TypeCheckResults: FSharp.Compiler.SourceCodeServices.FSharpCheckFileResults option }

    type BuildFailure = | InvalidProjectFileMessage of string

    /// Reason for the linter failing.
    [<NoComparison>]
    type LintFailure =
        /// Project file path did not exist on the local filesystem.
        | ProjectFileCouldNotBeFound of string

        /// Received exception when trying to get the list of F# file from the project file.
        | MSBuildFailedToLoadProjectFile of string * BuildFailure

        /// Failed to load a FSharpLint configuration file.
        | FailedToLoadConfig of string

        /// The specified file for linting could not be found.
        | FailedToLoadFile of string

        /// Failed to analyse a loaded FSharpLint configuration at runtime e.g. invalid hint.
        | RunTimeConfigError of string

        /// `FSharp.Compiler.Services` failed when trying to parse a file.
        | FailedToParseFile of ParseFile.ParseFileFailure

        /// `FSharp.Compiler.Services` failed when trying to parse one or more files in a project.
        | FailedToParseFilesInProject of ParseFile.ParseFileFailure list

        member Description: string

    /// Result of running the linter.
    [<NoEquality; NoComparison; RequireQualifiedAccess>]
    type LintResult =
        | Success of Suggestion.LintWarning list
        | Failure of LintFailure

        member TryGetSuccess : byref<Suggestion.LintWarning list> -> bool
        member TryGetFailure : byref<LintFailure> -> bool

    type Context =
        { IndentationRuleContext : Map<int,bool*int>
          NoTabCharactersRuleContext : (string * Range.range) list }

    /// Runs all rules which take a node of the AST as input.
    val runAstNodeRules : RuleMetadata<AstNodeRuleConfig> [] -> Rules.GlobalRuleConfig -> FSharpCheckFileResults option -> string -> string -> AbstractSyntaxArray.Node [] -> AbstractSyntaxArray.Skip [] -> Suggestion.LintWarning [] * Context

    /// Runs all rules which take a line of text as input.
    val runLineRules : LineRules -> Rules.GlobalRuleConfig -> string -> string -> Context -> Suggestion.LintWarning []

    /// Lints an entire F# solution by linting all projects specified in the `.sln` file.
    val lintSolution : optionalParams:OptionalLintParameters -> solutionFilePath:string -> LintResult

    /// Lints an entire F# project by retrieving the files from a given
    /// path to the `.fsproj` file.
    val lintProject : optionalParams:OptionalLintParameters -> projectFilePath:string -> LintResult

    /// Lints F# source code.
    val lintSource : optionalParams:OptionalLintParameters -> source:string -> LintResult

    /// Lints F# source code that has already been parsed using
    /// `FSharp.Compiler.Services` in the calling application.
    val lintParsedSource : optionalParams:OptionalLintParameters -> parsedFileInfo:ParsedFileInformation -> LintResult

    /// Lints an F# file from a given path to the `.fs` file.
    val lintFile : optionalParams:OptionalLintParameters -> filepath:string -> LintResult

    /// Lints an F# file that has already been parsed using
    /// `FSharp.Compiler.Services` in the calling application.
    val lintParsedFile : optionalParams:OptionalLintParameters -> parsedFileInfo:ParsedFileInformation -> filepath:string -> LintResult