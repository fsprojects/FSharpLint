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

module Tests =

    open NUnit.Framework

    type Error =
        {
            Description: string
            Location: string
            Code: string
        }

        override this.ToString() =
            sprintf "{\n    Description=\"%s\"\n    Location=\"%s\"\n    Code=\"%s\"\n}" this.Description this.Location this.Code

    let runConsoleApp arguments =
        let filename = 
            #if DEBUG
                @"../../../../src/FSharpLint.Console/bin/fsharplint.exe"
            #else
                @"../../../../bin/fsharplint.exe"
            #endif

        let startInfo = System.Diagnostics.ProcessStartInfo
                                (
                                    FileName = filename,
                                    Arguments = arguments,
                                    RedirectStandardOutput = true,
                                    UseShellExecute = false)

        use app = System.Diagnostics.Process.Start(startInfo)

        let output = app.StandardOutput.ReadToEnd()
                
        app.WaitForExit()

        output

    let getErrorsFromOutput (output:string) = 
        let splitOutput = output.Split([|System.Environment.NewLine|], System.StringSplitOptions.None)

        let errorIndexes = seq { for i in 0..splitOutput.Length / 4 - 1 -> 4 * i }

        [ for i in errorIndexes -> 
            {
                Description = splitOutput.[i]
                Location = splitOutput.[i + 1]
                Code = splitOutput.[i + 2]
            }
        ]

    let expectedErrors =
        [ "`not (a=b)` might be able to be refactored into `a<>b`."
          "`not (a<>b)` might be able to be refactored into `a=b`."
          "`fun x -> x` might be able to be refactored into `id`."
          "`not true` might be able to be refactored into `false`."
          "`not false` might be able to be refactored into `true`."
          "`List.fold + 0` might be able to be refactored into `List.sum`."
          "`a<>true` might be able to be refactored into `not a`."
          "`List.head (List.sort x)` might be able to be refactored into `List.min x`." ]
        
    [<TestFixture(Category = "Acceptance Tests")>]
    type TestConsoleApplication() =
        [<Test>]
        member this.InvalidConfig() = 
            let arguments = @"-f ../../../FSharpLint.FunctionalTest.TestedProject/FSharpLint.FunctionalTest.TestedProject.fsproj"

            System.IO.File.WriteAllText("../../../FSharpLint.FunctionalTest.TestedProject/Settings.FSharpLint", "invalid config file contents")

            let output = runConsoleApp arguments

            System.IO.File.Delete("../../../FSharpLint.FunctionalTest.TestedProject/Settings.FSharpLint")

            Assert.IsTrue(output.Contains("Failed to load config file"), sprintf "Output:\n%s" output)

        [<Test>]
        member this.FunctionsAsExpectedWithInvalidReferencedProjectFile() = 
            let projectFile = @"../../../FSharpLint.FunctionalTest.TestedProject/referencesInvalidProject.fsproj"

            let arguments = sprintf "-f %s" projectFile

            let output = runConsoleApp arguments
            
            let errors = getErrorsFromOutput output

            expectedErrors 
                |> List.iter (fun x -> Assert.True(List.exists (fun y -> y.Description = x) errors, 
                                                   "Errors did not contain expected error:\n" + x +
                                                   ". Program output:\n" + output))

            Assert.AreEqual(expectedErrors.Length, errors.Length)

        [<Test>]
        member this.InvalidProjectFile() = 
            let projectFile = @"../../../FSharpLint.FunctionalTest.TestedProject/invalidProjectFile.fsproj"

            let arguments = sprintf "-f %s" projectFile

            let output = runConsoleApp arguments
            
            Assert.IsTrue(output.StartsWith("MSBuild could not load the project file") && output.Contains("invalidProjectFile.fsproj"), sprintf "Output:\n%s" output)

        [<Test>]
        member this.UnableToFindProjectFile() = 
            let projectFile = @"../../../FSharpLint.FunctionalTest.TestedProject/iuniubi.fsproj"

            let arguments = sprintf "-f %s" projectFile

            let output = runConsoleApp arguments

            Assert.IsTrue(output.Contains(sprintf "Could not find the project file: %s on disk" projectFile), sprintf "Output:\n%s" output)

        [<Test>]
        member this.FunctionsAsExpectedWithNonExistantFindReferencedProjectFile() = 
            let projectFile = @"../../../FSharpLint.FunctionalTest.TestedProject/referencesNonExistantProject.fsproj"

            let arguments = sprintf "-f %s" projectFile

            let output = runConsoleApp arguments
            
            let errors = getErrorsFromOutput output

            expectedErrors 
                |> List.iter (fun x -> Assert.True(List.exists (fun y -> y.Description = x) errors, 
                                                   "Errors did not contain expected error:\n" + x +
                                                   ". Program output:\n" + output))

            Assert.AreEqual(expectedErrors.Length, errors.Length)

        [<Test>]
        member this.FunctionalTestConsoleApplication() = 
            let arguments = @"-f ../../../FSharpLint.FunctionalTest.TestedProject/FSharpLint.FunctionalTest.TestedProject.fsproj"

            let output = runConsoleApp arguments

            let errors = getErrorsFromOutput output

            expectedErrors 
                |> List.iter (fun x -> Assert.True(List.exists (fun y -> y.Description = x) errors, 
                                                   "Errors did not contain expected error:\n" + x +
                                                   ". Program output:\n" + output))

            Assert.AreEqual(expectedErrors.Length, errors.Length)