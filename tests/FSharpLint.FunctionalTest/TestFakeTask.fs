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

module TestFakeTask =

    open System.IO
    open NUnit.Framework

    let runFake() =
        let fakeExe = TestPackageHelper.getPath @"../../../../packages/FAKE/tools/FAKE.exe"

        let workingDirectory = TestPackageHelper.getPath @"../../../FSharpLint.FunctionalTest.TestedProject/"

        let buildFile =
            if Fake.EnvironmentHelper.isMono then
                System.IO.FileInfo(Path.Combine(workingDirectory, "testLintViaFake.fsx")).FullName
            else
                "testLintViaFake.fsx"

        let file = if Fake.EnvironmentHelper.isMono then "mono" else fakeExe

        let arguments =
            if Fake.EnvironmentHelper.isMono then
                sprintf "%s %s" (System.IO.FileInfo(fakeExe).FullName) buildFile
            else
                buildFile 

        let startInfo = System.Diagnostics.ProcessStartInfo
                                (
                                    FileName = file,
                                    Arguments = arguments,
                                    RedirectStandardOutput = true,
                                    WorkingDirectory = workingDirectory,
                                    UseShellExecute = false)

        use app = System.Diagnostics.Process.Start(startInfo)
                
        app.Start() |> ignore

        let output = System.Text.StringBuilder()
        
        while not app.StandardOutput.EndOfStream do
            app.StandardOutput.ReadLine() |> output.Append |> ignore

        output.ToString()
    
    [<TestFixture(Category = "Acceptance Tests")>]
    type TestFakeTask() =
        [<SetUp>]
        member this.CopyFSharpLintTaskFiles() = TestPackageHelper.copyFSharpLintTaskFiles "FSharpLintFakeTaskTest"

        [<Test>]
        member this.FunctionalTestFakeTask() = 
            let projectFile = TestPackageHelper.getPath @"../../../FSharpLint.FunctionalTest.TestedProject/FSharpLint.FunctionalTest.TestedProjectMSBuildTask.fsproj"

            let output = runFake()

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

            let allFound = List.forall (fun x -> output.Contains(x)) expectedErrors

            let failInfo = sprintf "FAKE output didn't contain expected lint warnings. output: %s" output
                
            Assert.IsTrue(allFound, failInfo)