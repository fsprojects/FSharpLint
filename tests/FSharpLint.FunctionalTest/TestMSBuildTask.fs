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

namespace FSharpLint.FunctionalTest

module TestMSBuildTask =

    open Microsoft.Build.Execution
    open Microsoft.Build.Framework
    open System.IO
    open NUnit.Framework
    
    [<TestFixture>]
    type TestMSBuildTask() =
        let getPath (path:string) = path.Replace('/', Path.DirectorySeparatorChar)

        let taskDirectory = getPath @"../../../FSharpLint.FunctionalTest.TestedProject/FSharpLint/"

        let taskBuildDirectory = Path.Combine(taskDirectory, "build")

        let copy toDirectory path = File.Copy(path, Path.Combine(toDirectory, Path.GetFileName(path)), true)

        [<SetUp>]
        member this.CopyFSharpLintTaskFiles() = 
            let taskDirectoryExists = Directory.Exists(taskDirectory)
            if not taskDirectoryExists then
                Directory.CreateDirectory(taskDirectory) |> ignore

            let copyToTaskDir = getPath >> (copy taskDirectory)

            let binDir = 
                #if DEBUG
                    "Debug"
                #else
                    "Release"
                #endif

            copyToTaskDir (@"../../../../src/FSharpLint.MSBuildIntegration/bin/" + binDir + "/FSharp.Core.dll")
            copyToTaskDir (@"../../../../src/FSharpLint.MSBuildIntegration/bin/" + binDir + "/FSharpLint.MSBuildIntegration.dll")
            copyToTaskDir (@"../../../../src/FSharpLint.MSBuildIntegration/bin/" + binDir + "/FSharpLint.Worker.dll")

            copyToTaskDir (@"../../../../src/FSharpLint.FAKE/bin/" + binDir + "/FSharpLint.FAKE.dll")
            copyToTaskDir (@"../../../../src/FSharpLint.CrossDomain/bin/" + binDir + "/FSharpLint.CrossDomain.dll")
            copyToTaskDir (@"../../../../src/FSharpLint.Application/bin/" + binDir + "/FSharpLint.Application.dll")
            copyToTaskDir (@"../../../../src/FSharpLint.Rules/bin/" + binDir + "/FSharpLint.Rules.dll")
            copyToTaskDir (@"../../../../src/FSharpLint.Rules/bin/" + binDir + "/FSharpLint.Framework.dll")
            copyToTaskDir (@"../../../../src/FSharpLint.Rules/bin/" + binDir + "/FSharp.Compiler.Service.dll")
            copyToTaskDir (@"../../../../src/FSharpLint.Rules/bin/" + binDir + "/FParsecCS.dll")
            copyToTaskDir (@"../../../../src/FSharpLint.Rules/bin/" + binDir + "/FParsec.dll")

            let taskBuildDirectoryExists = Directory.Exists(taskBuildDirectory)
            if not taskBuildDirectoryExists then
                Directory.CreateDirectory(taskBuildDirectory) |> ignore

            let copyToTaskBuildDir = getPath >> (copy taskBuildDirectory)

            copyToTaskBuildDir @"../../../../nugetpackage/build/FSharpLint.targets"

        [<Test>]
        member this.FunctionalTestMSBuildTask() = 
            let projectFile = getPath @"../../../FSharpLint.FunctionalTest.TestedProject/FSharpLint.FunctionalTest.TestedProjectMSBuildTask.fsproj"

            use buildManager = new BuildManager()

            let requestData = BuildRequestData(ProjectInstance(projectFile), [| "build" |])
            
            let lintErrors = System.Collections.Generic.List<BuildEventArgs>()
            let tasks = System.Collections.Generic.List<TaskStartedEventArgs>()

            let logger = 
                let parameters = ref ""
                let verbosity = ref LoggerVerbosity.Normal

                { new ILogger with 
                    member this.Parameters
                        with get () = !parameters
                        and set (value) = parameters := value

                    member this.Verbosity
                        with get () = !verbosity
                        and set (value) = verbosity := value

                    member this.Initialize(eventSource) = 
                        eventSource.WarningRaised.AddHandler(BuildWarningEventHandler(fun _ -> lintErrors.Add))

                        eventSource.TaskStarted.Add(tasks.Add)

                    member this.Shutdown() = ()
                }

            buildManager.Build(BuildParameters(Loggers = [logger]), requestData) |> ignore

            let errorMessages = lintErrors |> Seq.map (fun x -> x.Message) |> Seq.toList

            let expectedErrors =
                [
                    "not (a=b) can be refactored into a<>b"
                    "not (a<>b) can be refactored into a=b"
                    "fun x -> x can be refactored into id"
                    "not true can be refactored into false"
                    "not false can be refactored into true"
                    "List.fold + 0 can be refactored into List.sum"
                    "a<>true can be refactored into not a"
                    "List.head (List.sort x) can be refactored into List.min x"
                ]
                
            Assert.AreEqual(expectedErrors, errorMessages)