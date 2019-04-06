namespace FSharpLint.Application

/// Provides an API to manage/load FSharpLint configuration files.
/// <see cref="FSharpLint.Framework.Configuration" /> for more information on
/// the default configuration and overriding configurations.
module ConfigurationManagement =

    open FSharpLint.Framework.Configuration
    open FSharpLint.Application.ConfigurationManager
    
    type ConfigFailure =
        /// Failed to load a FSharpLint configuration file.
        | FailedToLoadConfig of string

        /// Failed to analyse a loaded FSharpLint configuration at runtime e.g. invalid hint.
        | RunTimeConfigError
        
    [<RequireQualifiedAccess; NoComparison>]
    type ConfigurationResult = 
        | Success of Configuration
        | Failure of ConfigFailure

    /// Load a FSharpLint configuration file from the contents (string) of the file.
    val loadConfigurationFile : configurationFileText:string -> Configuration

    /// Overrides the default FSharpLint configuration.
    /// The default FSharpLint configuration contains all required elements, so
    /// by overriding it any missing required elements will be added to the returned configuration.
    /// If you're loading your own configuration you should make sure that it overrides the default 
    /// configuration/overrides a configuration that has overriden the default configuration.
    val overrideDefaultConfiguration : configurationToOverrideDefault:string -> string

    /// Loads the FSharpLint configuration for a project given the path to the `.fsproj` file.
    /// It picks up configurations in any directory between the root directory and the project's directory.
    /// The closer they are to the project directory the higher precedence they have.
    /// e.g. if the project directory is C:\User\Matt\Project then a config file found in 
    /// C:\User\ will be loaded before and overridden by a config file found in C:\User\Matt\.
    val loadConfigurationForProject : projectFilePath:string -> ConfigurationResult

/// Provides an API for running FSharpLint from within another application. 
[<AutoOpen>]
module Lint =

    open System.Threading
    open FSharpLint.Framework

    /// Provides information on what the linter is currently doing.
    [<NoComparison>]
    type ProjectProgress =
        /// Started parsing a file (file path).
        | Starting of string

        /// Finished parsing a file (file path).
        | ReachedEnd of string * LintWarning.Warning list

        /// Failed to parse a file (file path, exception that caused failure).
        | Failed of string * System.Exception

        /// Path of the F# file the progress information is for.
        member FilePath : unit -> string

    /// Optional parameters that can be provided to the linter.
    [<NoEquality; NoComparison>]
    type OptionalLintParameters =
        { /// Cancels a lint in progress.
          CancellationToken: CancellationToken option

          /// Provide your own FSharpLint configuration to the linter.
          /// If not provided the default configuration will be used.
          Configuration: ConfigurationManager.Configuration option

          /// This function will be called every time the linter finds a broken rule.
          ReceivedWarning: (LintWarning.Warning -> unit) option 
          
          ReportLinterProgress: (ProjectProgress -> unit) option }

        static member Default: OptionalLintParameters

    /// If your application has already parsed the F# source files using `FSharp.Compiler.Services` 
    /// you want to lint then this can be used to provide the parsed information to prevent the 
    /// linter from parsing the file again.
    [<NoEquality; NoComparison>]
    type ParsedFileInformation =
        { /// File represented as an AST.
          Ast: FSharp.Compiler.Ast.ParsedInput

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

        /// Failed to analyse a loaded FSharpLint configuration at runtime e.g. invalid hint.
        | RunTimeConfigError

        /// `FSharp.Compiler.Services` failed when trying to parse a file.
        | FailedToParseFile of ParseFile.ParseFileFailure

        /// `FSharp.Compiler.Services` failed when trying to parse one or more files in a project.
        | FailedToParseFilesInProject of ParseFile.ParseFileFailure list

        member Description: string
        
    /// Result of running the linter.
    [<NoEquality; NoComparison; RequireQualifiedAccess>]
    type LintResult = 
        | Success of LintWarning.Warning list
        | Failure of LintFailure

        member TryGetSuccess : byref<LintWarning.Warning list> -> bool
        member TryGetFailure : byref<LintFailure> -> bool
        
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