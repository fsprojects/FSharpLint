(*
    FSharpLint, a linter for F#.
    Copyright (C) 2014 Matthew Mcveigh

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

namespace FSharpLint.Application

/// Provides an API to manage/load FSharpLint configuration files.
/// <see cref="FSharpLint.Framework.Configuration" /> for more information on
/// the default configuration and overriding configurations.
module ConfigurationManagement =

    open FSharpLint.Framework.Configuration
    
    type ConfigFailure =
        /// Failed to load a FSharpLint configuration file.
        | FailedToLoadConfig of string

        /// Failed to analyse a loaded FSharpLint configuration at runtime e.g. invalid hint.
        | RunTimeConfigError
        
    [<RequireQualifiedAccess>]
    type ConfigurationResult = 
        | Success of Configuration
        | Failure of ConfigFailure

    /// Load a FSharpLint configuration file from the contents (string) of the file.
    val loadConfigurationFile : configurationFileText:string -> Configuration

    /// Overrides a given FSharpLint configuration file with another.
    val overrideConfigurationFile : configurationToOverride:Configuration -> configurationToOverrideWith:Configuration -> Configuration

    /// Overrides the default FSharpLint configuration.
    /// The default FSharpLint configuration contains all required elements, so
    /// by overriding it any missing required elements will be added to the returned configuration.
    /// If you're loading your own configuration you should make sure that it overrides the default 
    /// configuration/overrides a configuration that has overriden the default configuration.
    val overrideDefaultConfiguration : configurationToOverrideDefault:Configuration -> Configuration

    /// Loads the FSharpLint configuration for a project given the path to the `.fsproj` file.
    /// It picks up configurations in any directory between the root directory and the project's directory.
    /// The closer they are to the project directory the higher precedence they have.
    /// e.g. if the project directory is C:\User\Matt\Project then a config file found in 
    /// C:\User\ will be loaded before and overridden by a config file found in C:\User\Matt\.
    val loadConfigurationForProject : projectFilePath:string -> ConfigurationResult

/// Provides an API for running FSharpLint from within another application. 
[<AutoOpen>]
module Lint =

    open FSharpLint.Framework

    /// Provides information on what the linter is currently doing.
    type ProjectProgress =
        /// Started parsing a file (file path).
        | Starting of string

        /// Finished parsing a file (file path).
        | ReachedEnd of string

        /// Failed to parse a file (file path, exception that caused failure).
        | Failed of string * System.Exception

        /// Path of the F# file the progress information is for.
        member FilePath : unit -> string

    /// Optional parameters that can be provided to the linter.
    type OptionalLintParameters =
        {
            /// This function will be called as the linter progresses through the AST of each file.
            /// The linter will stop linting if this function returns true.
            FinishEarly: (unit -> bool) option

            /// Provide your own FSharpLint configuration to the linter.
            /// If not provided the default configuration will be used.
            Configuration: Configuration.Configuration option

            /// This function will be called every time the linter finds a broken rule.
            ReceivedWarning: (LintWarning.Warning -> unit) option
        }

        static member Default: OptionalLintParameters

    /// If your application has already parsed the F# source files using `FSharp.Compiler.Services` 
    /// you want to lint then this can be used to provide the parsed information to prevent the 
    /// linter from parsing the file again.
    type ParsedFileInformation =
        {
            /// File represented as an AST.
            Ast: Microsoft.FSharp.Compiler.Ast.ParsedInput

            /// Contents of the file.
            Source: string

            /// Optional results of inferring the types on the AST (allows for a more accurate lint).
            TypeCheckResults: Microsoft.FSharp.Compiler.SourceCodeServices.FSharpCheckFileResults option

            /// Version of F# the source code of the file was written in.
            FSharpVersion: System.Version
        }

    /// Reason for the linter failing.
    type LintFailure =
        /// Project file path did not exist on the local filesystem.
        | ProjectFileCouldNotBeFound of string

        /// Received exception when trying to get the list of F# file from the project file.
        | MSBuildFailedToLoadProjectFile of string * Microsoft.Build.Exceptions.InvalidProjectFileException

        /// Failed to load a FSharpLint configuration file.
        | FailedToLoadConfig of string

        /// Failed to analyse a loaded FSharpLint configuration at runtime e.g. invalid hint.
        | RunTimeConfigError

        /// `FSharp.Compiler.Services` failed when trying to parse a file.
        | FailedToParseFile of ParseFile.ParseFileFailure

        /// `FSharp.Compiler.Services` failed when trying to parse one or more files in a project.
        | FailedToParseFilesInProject of ParseFile.ParseFileFailure list
        
    /// Result of running the linter.
    [<RequireQualifiedAccess>]
    type LintResult = 
        | Success of LintWarning.Warning list
        | Failure of LintFailure

    /// Lints an entire F# project by retrieving the files from a given 
    /// path to the `.fsproj` file.
    val lintProject : optionalParams:OptionalLintParameters -> projectFilePath:string -> progress:(ProjectProgress -> unit) option -> LintResult

    /// Lints F# source code.
    val lintSource : optionalParams:OptionalLintParameters -> source:string -> fsharpVersion:System.Version -> LintResult

    /// Lints F# source code that has already been parsed using 
    /// `FSharp.Compiler.Services` in the calling application.
    val lintParsedSource : optionalParams:OptionalLintParameters -> parsedFileInfo:ParsedFileInformation -> LintResult

    /// Lints an F# file from a given path to the `.fs` file.
    val lintFile : optionalParams:OptionalLintParameters -> filepath:string -> fsharpVersion:System.Version -> LintResult

    /// Lints an F# file that has already been parsed using 
    /// `FSharp.Compiler.Services` in the calling application. 
    val lintParsedFile : optionalParams:OptionalLintParameters -> parsedFileInfo:ParsedFileInformation -> filepath:string -> LintResult