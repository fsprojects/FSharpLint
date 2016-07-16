// FSharpLint, a linter for F#.
// Copyright (C) 2016 Matthew Mcveigh
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace FSharpLint.FunctionalTest

module TestApi =

    open System
    open System.IO
    open System.Diagnostics
    open NUnit.Framework
    open FSharpLint.Application.Lint
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open TestPackageHelper

    let sourceFile = basePath </> "tests" </> "TypeChecker.fs"
    
    [<TestFixture(Category = "Acceptance Tests")>]
    type TestApi() =
        let generateAst source =
            let checker = FSharpChecker.Create()

            let options = 
                checker.GetProjectOptionsFromScript(sourceFile, source) 
                |> Async.RunSynchronously

            let parseResults =
                checker.ParseFileInProject(sourceFile, source, options)
                |> Async.RunSynchronously
        
            match parseResults.ParseTree with
            | Some(parseTree) -> parseTree
            | None -> failwith "Failed to parse file."

        [<Category("Performance")>]
        [<Test>]
        member __.``Performance of linting an existing file``() = 
            let text = File.ReadAllText sourceFile
            let tree = text |> generateAst
            let fileInfo = { Ast = tree; Source = text; TypeCheckResults = None; FSharpVersion = Version(4, 0) }
            
            let stopwatch = Stopwatch.StartNew()
            let times = ResizeArray()

            let iterations = 100

            for _ in 0..iterations do
                stopwatch.Restart()
                
                lintParsedFile OptionalLintParameters.Default fileInfo sourceFile |> ignore

                stopwatch.Stop()

                times.Add stopwatch.ElapsedMilliseconds

            let result = times |> Seq.sum |> (fun totalMilliseconds -> totalMilliseconds / int64 iterations)
            
            Assert.Less(result, 250)
            System.Console.WriteLine(sprintf "Average runtime of linter on parsed file: %d (milliseconds)."  result)