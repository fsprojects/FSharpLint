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

module ProjectFile =

    open System.Linq
    open Microsoft.Build.Execution
    open Microsoft.Build.Tasks
    open Microsoft.Build.Framework
    open Microsoft.Build.BuildEngine
    open Microsoft.FSharp.Compiler.SourceCodeServices 
    
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

    let parseProject (projectFile:string) = 
        let p = ProjectInstance(projectFile)

        let p = p.Items

        let compiles = 
            p
                |> Seq.filter (fun x -> x.ItemType = "Compile")
                |> Seq.map (fun x -> x.EvaluatedInclude.ToString())
                |> Seq.toList

        let resolve = ResolveAssemblyReference()
        resolve.BuildEngine <- stubBuildEngine

        let references = p |> Seq.filter (fun x -> x.ItemType = "Reference" || x.ItemType = "ProjectReference") 

        resolve.TargetFrameworkVersion <- "v4.0"

        let frameworkMoniker = System.Runtime.Versioning.FrameworkName(".NETFramework", System.Version("4.0"))
        let referenceAssemblies = Microsoft.Build.Utilities.ToolLocationHelper.GetPathToReferenceAssemblies(frameworkMoniker)

        resolve.TargetFrameworkDirectories <- referenceAssemblies.ToArray()

        resolve.AppConfigFile <- @"C:\Users\matthewm\Documents\GitHub\FSharpLint\src\FSharpLint.Console\App.config"

        resolve.SearchPaths <- 
            [|
                "{CandidateAssemblyFiles}"
                "{HintPathFromItem}"
                "{TargetFrameworkDirectory}"
                "{Registry:$(FrameworkRegistryBase),$(TargetFrameworkVersion),$(AssemblyFoldersSuffix)$(AssemblyFoldersExConditions)}"
                "{AssemblyFolders}"
                "{GAC}"
                "{RawFileName}"
                @"C:\Users\matthewm\Documents\GitHub\FSharpLint\src\FSharpLint.Console\bin"
            |]

        resolve.Assemblies <- references |> Seq.map (fun x -> (x :> ITaskItem)) |> Seq.toArray
        
        resolve.Execute() |> ignore

        //let projectreferences = p |> Seq.filter (fun x -> x.ItemType = "ProjectReference")

        let checker = InteractiveChecker.Create()

        let files = 
            [
                "C:\Users\matthewm\Documents\GitHub\FSharpLint\src\FSharpLint.Console\ErrorHandling.fs"
                "C:\Users\matthewm\Documents\GitHub\FSharpLint\src\FSharpLint.Console\Program.fs"
            ]
        
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
                   //for c in compiles do yield c
                   for c in files do yield c
                   for r in resolve.ResolvedFiles do yield "-r:" + r.ItemSpec.ToString()
                   yield "-r:FSharpLint.Framework.dll"
                   yield "-r:FSharpLint.Rules.dll"
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
            ]

            FSharpLint.Framework.Ast.parse checker projectOptions file input visitors |> ignore

        files |> List.iter parseFile