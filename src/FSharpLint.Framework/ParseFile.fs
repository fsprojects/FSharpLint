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

namespace FSharpLint.Framework

/// Provides functionality to parse F# files using `FSharp.Compiler.Services`.
module ParseFile = 

    open FSharpLint.Framework
    open FSharpLint.Framework.Configuration
    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.SourceCodeServices

    type ParseFileFailure =
        | FailedToParseFile of FSharpErrorInfo []
        | AbortedTypeCheck

    type ParseFileResult<'t> =
        | Failed of ParseFileFailure
        | Success of 't
        
    let private parse configuration file source (checker:FSharpChecker, options) =
        let parseResults = 
            checker.ParseFileInProject(file, source, options)
            |> Async.RunSynchronously

        let typeCheckFile () =
            match configuration.UseTypeChecker with
            | Some(true) ->
                let results = 
                    checker.CheckFileInProject(parseResults, file, 0, source, options) 
                    |> Async.RunSynchronously

                match results with
                | FSharpCheckFileAnswer.Succeeded(x) -> Success(Some(x))
                | FSharpCheckFileAnswer.Aborted -> Failed(AbortedTypeCheck)
            | Some(false) | None -> Success(None)

        match parseResults.ParseTree with
        | Some(parseTree) -> 
            match typeCheckFile() with
            | Success(typeCheckResults) ->
                { Ast.PlainText = source
                  Ast.Ast = parseTree
                  Ast.TypeCheckResults = typeCheckResults
                  Ast.File = file } |> Success
            | Failed(_) -> Failed(AbortedTypeCheck)
        | None -> Failed(FailedToParseFile(parseResults.Errors))

    /// Parses a file using `FSharp.Compiler.Services`.
    let parseFile file configuration (checker:FSharpChecker) options =
        let source = System.IO.File.ReadAllText(file)

        let options =
            match options with
            | Some(existingOptions) -> existingOptions
            | None -> 
                checker.GetProjectOptionsFromScript(file, source) 
                |> Async.RunSynchronously
        
        parse configuration file source (checker, options)
        
    /// Parses source code using `FSharp.Compiler.Services`.
    let parseSource source configuration (checker:FSharpChecker) =
        let file = "/home/user/Dog.test.fsx"
        
        let options = 
            checker.GetProjectOptionsFromScript(file, source) 
            |> Async.RunSynchronously

        parse configuration file source (checker, options)