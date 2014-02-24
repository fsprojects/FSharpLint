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

namespace FSharpLint.Console

/// <summary>Runs the lint on an entire project using a .fsproj file.</summary>
/// <remarks>
/// Depends on MSBuild classes that are appearantly obselete.
/// Probably doesn't handle all cases, as such this is temporary
/// just so that the lint is a workable tool, will be reworked
/// once there's a fair amount of rules completed as those have 
/// a higher priority. Probably end up using ReferenceResolution.fs
/// from fsharp.
/// </remarks>
module ProjectFile =

    open System.Linq
    open Microsoft.Build.Execution
    open Microsoft.Build.Tasks
    open Microsoft.Build.Framework
    open Microsoft.Build.BuildEngine
    open Microsoft.FSharp.Compiler.SourceCodeServices 
    
    /// An instance of the IBuildEngine is required for ResolveAssemblyReference.
    /// We don't need it to do anything.
    let stubBuildEngine = 
        { new IBuildEngine with
            member this.BuildProjectFile(_, _, _, _) = true

            member this.ColumnNumberOfTaskNode with get() = 0

            member this.ContinueOnError with get() = false

            member this.LineNumberOfTaskNode with get() = 0

            member this.ProjectFileOfTaskNode with get() = ""

            member this.LogCustomEvent(_) = ()

            member this.LogErrorEvent(_) = ()

            member this.LogMessageEvent(_) = ()

            member this.LogWarningEvent(_) = ()
        }

    /// Default paths to looks for references within. Used by ResolveAssemblyReference.
    let defaultSearchPaths extraSearchPaths =
        [
            "{CandidateAssemblyFiles}"
            "{HintPathFromItem}"
            "{TargetFrameworkDirectory}"
            "{Registry:$(FrameworkRegistryBase),$(TargetFrameworkVersion),$(AssemblyFoldersSuffix)$(AssemblyFoldersExConditions)}"
            "{AssemblyFolders}"
            "{GAC}"
            "{RawFileName}"
        ] @ extraSearchPaths |> List.toArray

    /// Resolves a a list of references from their short term form e.g. System.Core to absolute paths to the dlls.
    let resolveReferences (projectInstance:ProjectInstance) outputPath references =
        let resolve = ResolveAssemblyReference()
        resolve.BuildEngine <- stubBuildEngine

        resolve.TargetFrameworkVersion <- projectInstance.ToolsVersion

        resolve.SearchPaths <- defaultSearchPaths [outputPath]

        resolve.Assemblies <- references |> Seq.map (fun x -> (x :> ITaskItem)) |> Seq.toArray

        let frameworkMoniker = System.Runtime.Versioning.FrameworkName(".NETFramework", System.Version(projectInstance.ToolsVersion))
        let referenceAssemblies = Microsoft.Build.Utilities.ToolLocationHelper.GetPathToReferenceAssemblies(frameworkMoniker)

        resolve.TargetFrameworkDirectories <- referenceAssemblies.ToArray()
        
        resolve.Execute() |> ignore

        resolve.ResolvedFiles |> Seq.map (fun x -> x.ItemSpec.ToString()) |> Seq.toList

    /// Paths of all required files used to construct project options (must be absolute paths).
    type ProjectFile = 
        {
            References: string list
            ProjectReferences: string list
            FSharpFiles: string list
        }

    let getOutputRelativePath (projectInstance:ProjectInstance) =
        projectInstance.Items 
            |> Seq.filter (fun x -> x.ItemType = "_OutputPathItem") 
            |> Seq.head 
            |> fun x -> x.ToString()

    let getProjectReferences (projectInstance:ProjectInstance) projectPath =
        projectInstance.Items 
            |> Seq.filter (fun x -> x.ItemType = "ProjectReference")
            |> Seq.collect (fun x -> ProjectInstance(System.IO.Path.Combine(projectPath, x.ToString())).Items)
            |> Seq.filter (fun x -> x.ItemType = "BuiltProjectOutputGroupKeyOutput")
            |> Seq.map (fun x -> x.ToString())

    let getReferences (projectInstance:ProjectInstance) =
        projectInstance.Items |> Seq.filter (fun x -> x.ItemType = "Reference")

    /// Gets a list of the .fs and .fsi files in the project.
    let getFSharpFiles (projectInstance:ProjectInstance) projectPath =
        projectInstance.Items 
            |> Seq.filter (fun item -> item.ItemType = "Compile")
            |> Seq.map (fun item -> item.EvaluatedInclude.ToString())
            |> Seq.toList
            |> List.map (fun x -> System.IO.Path.Combine(projectPath, x.ToString()))

    let getProjectFiles (projectFile:string) =
        let projectPath = System.IO.Path.GetDirectoryName(projectFile)

        let projectInstance = ProjectInstance(projectFile)

        let references = getReferences projectInstance

        let projectReferences = getProjectReferences projectInstance projectPath

        let outputAbsolutePath = System.IO.Path.Combine(projectPath, getOutputRelativePath projectInstance)

        {
            References = getReferences projectInstance |> resolveReferences projectInstance outputAbsolutePath
            ProjectReferences = projectReferences |> Seq.toList
            FSharpFiles = getFSharpFiles projectInstance projectPath
        }
        
    /// <summary>Parses and runs the linter on all the files in a project.</summary>
    /// <param name="projectFile">Absolute path to the .fsproj file.</param>
    let parseProject (projectFile:string) = 
        let projectFileValues = getProjectFiles projectFile

        let checker = InteractiveChecker.Create()
        
        let projectOptions = 
            checker.GetProjectOptionsFromCommandLineArgs
               (projectFile,
                [| yield "--simpleresolution" 
                   yield "--noframework" 
                   yield "--debug:full" 
                   yield "--define:DEBUG" 
                   yield "--optimize-" 
                   yield "--out:" + "dog.exe"
                   yield "--doc:test.xml" 
                   yield "--warn:3" 
                   yield "--fullpaths" 
                   yield "--flaterrors" 
                   yield "--target:exe" 
                   yield! projectFileValues.FSharpFiles
                   for r in projectFileValues.References do yield "-r:" + r
                   for r in projectFileValues.ProjectReferences do yield "-r:" + r
                |])

        let parseFile file =
            let input = System.IO.File.ReadAllText(file);

            let postError range error =
                ErrorHandling.errorHandler.Post(
                    {
                        Info = error
                        Range = range
                        Input = input
                    })

            let visitors = [
                FSharpLint.Rules.NameConventions.visitor postError
                FSharpLint.Rules.FavourIgnoreOverLetWild.visitor postError
                FSharpLint.Rules.FunctionParametersLength.visitor postError
                FSharpLint.Rules.XmlDocumentation.visitor postError
            ]

            FSharpLint.Framework.Ast.parse checker projectOptions file input visitors |> ignore

        projectFileValues.FSharpFiles |> List.iter parseFile