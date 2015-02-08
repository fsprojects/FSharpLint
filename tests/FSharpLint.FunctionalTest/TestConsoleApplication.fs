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
                
        app.WaitForExit()

        app.StandardOutput.ReadToEnd()

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

    let isRunningOnUnixBasedSystem =
        System.Environment.OSVersion.Platform = System.PlatformID.MacOSX ||
        System.Environment.OSVersion.Platform = System.PlatformID.Unix

    let toPlatformSpecificPath error = 
        if isRunningOnUnixBasedSystem then
            { error with Location = error.Location.Replace('\\', '/') }
        else
            error

    [<TestFixture>]
    type TestConsoleApplication() =
        [<Test>]
        member this.InvalidConfig() = 
            let arguments = @"-f ../../../FSharpLint.FunctionalTest.TestedProject/FSharpLint.FunctionalTest.TestedProject.fsproj"

            System.IO.File.WriteAllText("../../../FSharpLint.FunctionalTest.TestedProject/Settings.FSharpLint", "invalid config file contents")

            let output = runConsoleApp arguments

            System.IO.File.Delete("../../../FSharpLint.FunctionalTest.TestedProject/Settings.FSharpLint")

            Assert.IsTrue(output.Contains("Failed to load config file"))

        [<Test>]
        member this.InvalidReferencedProjectFile() = 
            let projectFile = @"../../../FSharpLint.FunctionalTest.TestedProject/referencesInvalidProject.fsproj"

            let arguments = sprintf "-f %s" projectFile

            let output = runConsoleApp arguments

            Assert.IsTrue(
                output.Contains("MSBuild could not load a referenced project file") && 
                output.Contains("invalidProjectFile.fsproj"))

        [<Test>]
        member this.InvalidProjectFile() = 
            let projectFile = @"../../../FSharpLint.FunctionalTest.TestedProject/invalidProjectFile.fsproj"

            let arguments = sprintf "-f %s" projectFile

            let output = runConsoleApp arguments
            
            Assert.IsTrue(output.Contains(sprintf "MSBuild could not load the project file %s" projectFile))

        [<Test>]
        member this.UnableToFindProjectFile() = 
            let projectFile = @"../../../FSharpLint.FunctionalTest.TestedProject/iuniubi.fsproj"

            let arguments = sprintf "-f %s" projectFile

            let output = runConsoleApp arguments

            Assert.IsTrue(output.Contains(sprintf "Could not find the project file: %s on disk" projectFile))

        [<Test>]
        member this.UnableToFindReferencedProjectFile() = 
            let projectFile = @"../../../FSharpLint.FunctionalTest.TestedProject/referencesNonExistantProject.fsproj"

            let arguments = sprintf "-f %s" projectFile

            let output = runConsoleApp arguments

            Assert.IsTrue(output.Contains("Could not find file"))

        [<Test>]
        member this.FunctionalTestConsoleApplication() = 
            let arguments = @"-f ../../../FSharpLint.FunctionalTest.TestedProject/FSharpLint.FunctionalTest.TestedProject.fsproj"

            let output = runConsoleApp arguments

            let errors = getErrorsFromOutput output

            let expectedErrors =
                [
                    { 
                        Description = "not (a=b) can be refactored into a<>b" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 6 starting at column 4"
                        Code = "    not ((x * y) = z) |> ignore"
                    }
                    { 
                        Description = "not (a<>b) can be refactored into a=b" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 8 starting at column 15"
                        Code = "    let meow = not (1 <> 1)"
                    }
                    { 
                        Description = "fun x -> x can be refactored into id" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 10 starting at column 13"
                        Code = "    let id = fun x -> x"
                    }
                    { 
                        Description = "not true can be refactored into false" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 12 starting at column 14"
                        Code = "    let dog = not true"
                    }
                    { 
                        Description = "not false can be refactored into true" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 14 starting at column 14"
                        Code = "    let dog = not false"
                    }
                    { 
                        Description = "List.fold + 0 can be refactored into List.sum" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 16 starting at column 25"
                        Code = "    let sum = [1;2;3] |> List.fold (+) 0"
                    }
                    { 
                        Description = "a<>true can be refactored into not a" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 20 starting at column 7"
                        Code = "    if x <> true then"
                    }
                    { 
                        Description = "List.head (List.sort x) can be refactored into List.min x" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 23 starting at column 15"
                        Code = "    let woof = [1;2;3] |> List.sort |> List.head"
                    }
                ]

            expectedErrors 
                |> List.map toPlatformSpecificPath
                |> List.iter (fun x -> Assert.True(List.exists ((=) x) errors, 
                                                   "Errors did not contain expected error:\n" + x.ToString() +
                                                   ". Program output:\n" + output))

            Assert.AreEqual(expectedErrors.Length, errors.Length)
