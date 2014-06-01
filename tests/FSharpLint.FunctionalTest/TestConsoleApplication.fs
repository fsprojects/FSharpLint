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

        let output = app.StandardOutput.ReadToEnd().Split([|System.Environment.NewLine|], System.StringSplitOptions.None)

        let errorIndexes = seq { for i in 0..output.Length / 4 - 1 -> 4 * i }

        [ for i in errorIndexes -> 
            {
                Description = output.[i]
                Location = output.[i + 1]
                Code = output.[i + 2]
            }
        ]

    [<TestFixture>]
    type TestConsoleApplication() =

        [<Test>]
        member this.FunctionalTestConsoleApplication() = 
            let arguments = @"-f ..\..\..\FSharpLint.FunctionalTest.TestedProject\FSharpLint.FunctionalTest.TestedProject.fsproj"

            let errors = runConsoleApp arguments

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
                        Description = "not True can be refactored into False" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 12 starting at column 14"
                        Code = "    let dog = not true"
                    }
                    { 
                        Description = "not False can be refactored into True" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 14 starting at column 14"
                        Code = "    let dog = not false"
                    }
                    { 
                        Description = "List.fold + 0 can be refactored into List.sum" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 16 starting at column 25"
                        Code = "    let sum = [1;2;3] |> List.fold (+) 0"
                    }
                    { 
                        Description = "a<>True can be refactored into not a" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 20 starting at column 7"
                        Code = "    if x <> true then"
                    }
                    { 
                        Description = "sin x/cos x can be refactored into tan x" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 23 starting at column 15"
                        Code = "    let meow = sin 4.0 / cos 4.0"
                    }
                    { 
                        Description = "List.head (List.sort x) can be refactored into List.min x" 
                        Location = "Error in file ..\\..\\..\\FSharpLint.FunctionalTest.TestedProject\\TestHints.fs on line 25 starting at column 15"
                        Code = "    let woof = [1;2;3] |> List.sort |> List.head"
                    }
                ]

            expectedErrors 
                |> List.iter (fun x -> Assert.True(List.exists ((=) x) errors, "Errors did not contain expected error:\n" + x.ToString()))

            Assert.AreEqual(expectedErrors.Length, errors.Length)
