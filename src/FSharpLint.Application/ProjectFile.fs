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

module ProjectFile =

    open System.Linq
    open Microsoft.Build.Tasks
    open Microsoft.Build.Framework
    open Microsoft.Build.BuildEngine
    open Microsoft.FSharp.Compiler.SourceCodeServices 
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let SettingsFileName = "Settings.FSharpLint"

    type Error =
        | ProjectFileCouldNotBeFound of string
        | MSBuildFailedToLoadProjectFile of string * Microsoft.Build.Exceptions.InvalidProjectFileException
        | MSBuildFailedToLoadReferencedProjectFile of string * Microsoft.Build.Exceptions.InvalidProjectFileException
        | UnableToFindProjectOutputPath of string
        | UnableToFindReferencedProject of string
        | UnableToFindFSharpCoreDirectory
        | FailedToLoadConfig of string
        | RunTimeConfigError
        | FailedToResolveReferences

    type Result<'TSuccess> = 
        | Success of 'TSuccess
        | Failure of Error

    /// Paths of all required files used to construct project options (must be absolute paths).
    type ProjectFile = 
        {
            Path: string
            References: string list
            ProjectReferences: string list
            FSharpFiles: string list
            Config: Map<string,Analyser>
        }

    /// Resolves a a list of references from their short term form e.g. System.Core to absolute paths to the dlls.
    let private resolveReferences (projectInstance:Microsoft.Build.Evaluation.Project) outputPath references =
        let references = references 
                            |> Seq.map (fun (x:Microsoft.Build.Evaluation.ProjectItem) -> (x.EvaluatedInclude, ""))
                            |> Seq.toArray

        let fsharpCoreDirectory = Microsoft.FSharp.Compiler.MSBuildResolver.DotNetFrameworkReferenceAssembliesRootDirectory
                                      + @"\..\..\FSharp\{0}\Runtime\v4.0"

        let altDirectory = Microsoft.FSharp.Compiler.MSBuildResolver.DotNetFrameworkReferenceAssembliesRootDirectory
                                + @"\..\..\FSharp\.NETFramework\v4.0\4.3.0.0\"

        let isRunningOnMono() = System.Type.GetType("Mono.Runtime") <> null

        let fsharpCoreDirectory =
            if System.IO.Directory.Exists(System.String.Format(fsharpCoreDirectory, "3.1")) then
                Success(System.String.Format(fsharpCoreDirectory, "3.1"))
            else if System.IO.Directory.Exists(System.String.Format(fsharpCoreDirectory, "3.0")) then
                Success(System.String.Format(fsharpCoreDirectory, "3.0"))
            else if System.IO.Directory.Exists(altDirectory) then
                Success(altDirectory)
            else if isRunningOnMono() then
                Success(System.String.Format(fsharpCoreDirectory, "3.1"))
            else
                Failure(UnableToFindFSharpCoreDirectory)

        match fsharpCoreDirectory with
            | Success(fsharpCoreDirectory) ->
                let resolvedReferences = 
                    try
                        Microsoft.FSharp.Compiler.MSBuildResolver.Resolve(
                                Microsoft.FSharp.Compiler.MSBuildResolver.CompileTimeLike, 
                                references,
                                "v" + projectInstance.ToolsVersion,
                                [],
                                "",
                                outputPath,
                                fsharpCoreDirectory,
                                [],
                                "",
                                "",
                                "",
                                "",
                                (fun _ -> ()),
                                (fun _ _ -> ()),
                                (fun _ _ -> ())
                            ) |> Success
                    with
                        | :? Microsoft.FSharp.Compiler.MSBuildResolver.ResolutionFailure ->
                            Failure(FailedToResolveReferences)

                match resolvedReferences with
                    | Success(resolvedReferences) ->
                        resolvedReferences.resolvedFiles 
                            |> Seq.map (fun x -> x.itemSpec) 
                            |> Seq.toList
                            |> Success
                    | Failure(x) -> Failure(x)
            | Failure(x) -> Failure(x)

    let openProjectFile (projectFile:string) =
        try
            let xmlReader = System.Xml.XmlReader.Create(projectFile)

            Microsoft.Build.Evaluation.Project(xmlReader) |> Success
        with
            | :? Microsoft.Build.Exceptions.InvalidProjectFileException as e ->
                Failure(MSBuildFailedToLoadProjectFile(projectFile, e))
            | :? System.Security.SecurityException
            | :? System.IO.FileNotFoundException
            | :? System.UriFormatException ->
                Failure(ProjectFileCouldNotBeFound(projectFile))

    exception ReferencedProjectFileException of Error

    let getProjectReferences (projectInstance:Microsoft.Build.Evaluation.Project) projectPath =
        try 
            let getReferencedProjectOutputItems (x:Microsoft.Build.Evaluation.ProjectItem) =
                let openedProject = System.IO.Path.Combine(projectPath, x.EvaluatedInclude) |> openProjectFile

                match openedProject with
                    | Success(project) ->
                        project.GetItems("BuiltProjectOutputGroupKeyOutput") |> Seq.toList
                    | Failure(error) -> 
                        raise <| ReferencedProjectFileException error

            projectInstance.GetItems("ProjectReference")
                |> Seq.toList
                |> List.collect getReferencedProjectOutputItems
                |> List.map (fun x -> x.ToString())
                |> Success
        with
            | ReferencedProjectFileException(error) ->
                Failure <|
                    match error with
                        | MSBuildFailedToLoadProjectFile(p, e) -> MSBuildFailedToLoadReferencedProjectFile(p, e)
                        | ProjectFileCouldNotBeFound(p) -> UnableToFindReferencedProject(p)
                        | x -> x

    /// Gets a list of the .fs and .fsi files in the project.
    let getFSharpFiles (projectInstance:Microsoft.Build.Evaluation.Project) projectPath =
        projectInstance.GetItems("Compile")
            |> Seq.map (fun item -> System.IO.Path.Combine(projectPath, item.EvaluatedInclude))
            |> Seq.toList

    let loadConfigForProject projectFilePath =
        let configCheckers = 
            System.Reflection.Assembly.Load("FSharpLint.Rules")
                |> FSharpLint.Framework.LoadAnalysers.loadConfigCheckers

        let checkConfig config =
            let configFailures = configCheckers 
                                    |> (FSharpLint.Framework.LoadAnalysers.checkConfigsForFailures config)

            if List.length configFailures = 0 then
                config |> Success
            else
                Failure(FailedToLoadConfig(List.head configFailures))


        let config = 
            try
                loadDefaultConfiguration() |> checkConfig
            with
                | ConfigurationException(message) ->
                    Failure(FailedToLoadConfig ("Failed to load default config: " + message))

        match config with
            | Success(config) ->
                let projectConfigPath = System.IO.Path.GetDirectoryName(projectFilePath)

                let filename = System.IO.Path.Combine(projectConfigPath, SettingsFileName)

                if projectConfigPath <> null && System.IO.File.Exists(filename) then
                    try
                        overrideConfiguration config filename |> checkConfig
                    with
                        | ConfigurationException(message) ->
                            Failure(FailedToLoadConfig (sprintf "Failed to load config file %s: %s" filename message))
                        | :? System.Xml.XmlException as e ->
                            Failure(FailedToLoadConfig (sprintf "Failed to load config file %s: %s" filename e.Message))
                else
                    Success(config)
            | x -> x

    let loadProjectFile (projectFile:string) =
        match openProjectFile projectFile with
            | Success(projectInstance) ->
                let projectPath = System.IO.Path.GetDirectoryName(projectFile)

                match getProjectReferences projectInstance projectPath with
                    | Success(projectReferences) ->

                        let outputProperty = projectInstance.GetProperty("OutputPath")

                        if outputProperty = null then
                            Failure(UnableToFindProjectOutputPath projectFile)
                        else
                            let outputAbsolutePath = System.IO.Path.Combine(projectPath, outputProperty.EvaluatedValue)

                            let config = loadConfigForProject projectFile

                            match loadConfigForProject projectFile with
                                | Success(config) ->
                                    match projectInstance.GetItems("Reference") |> resolveReferences projectInstance outputAbsolutePath with
                                        | Success(references) ->
                                            {
                                                Path = projectFile
                                                References = references
                                                ProjectReferences = projectReferences
                                                FSharpFiles = getFSharpFiles projectInstance projectPath
                                                Config = config
                                            } |> Success

                                        | Failure(error) -> Failure(error)
                                | Failure(error) -> Failure(error)
                    | Failure(error) -> Failure(error)
            | Failure(error) -> Failure(error)