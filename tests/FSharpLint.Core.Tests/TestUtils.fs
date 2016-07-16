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

/// Holds utility code common to multiple test files.
module TestUtils

    open System.IO
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open NUnit.Framework

    let (</>) x y = Path.Combine(x, y)

    let private basePath = TestContext.CurrentContext.TestDirectory </> ".." </> ".." </> ".." </> ".."

    let private performanceTestSourceFile = basePath </> "tests" </> "TypeChecker.fs"

    let generateAst source =
        let checker = FSharpChecker.Create()

        let options = 
            checker.GetProjectOptionsFromScript(performanceTestSourceFile, source) 
            |> Async.RunSynchronously

        let parseResults =
            checker.ParseFileInProject(performanceTestSourceFile, source, options)
            |> Async.RunSynchronously
        
        match parseResults.ParseTree with
        | Some(parseTree) -> parseTree
        | None -> failwith "Failed to parse file."

    let getPerformanceTestInput =
        let memoizedResult = ref None

        fun () ->
            match !memoizedResult with
            | Some(result) -> result
            | None ->
                let text = performanceTestSourceFile |> File.ReadAllText
                let result = generateAst text, text
                memoizedResult := Some(result)
                result