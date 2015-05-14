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
        
    let private parse configuration file source =
        let checker = FSharpChecker.Create()
        
        let options = 
            checker.GetProjectOptionsFromScript(file, source) 
                |> Async.RunSynchronously

        let parseResults = 
            checker.ParseFileInProject(file, source, options)
                |> Async.RunSynchronously

        let typeCheckFile () =
            if configuration.UseTypeChecker then
                let results = 
                    checker.CheckFileInProject(parseResults, file, 0, source, options) 
                        |> Async.RunSynchronously

                match results with
                    | FSharpCheckFileAnswer.Succeeded(x) -> 
                        Success(Some(x))
                    | FSharpCheckFileAnswer.Aborted -> 
                        Failed(AbortedTypeCheck)
            else
                Success(None)

        match parseResults.ParseTree with
            | Some(parseTree) -> 
                match typeCheckFile() with
                    | Success(typeCheckResults) ->
                        {
                            Ast.PlainText = source
                            Ast.Ast = parseTree
                            Ast.TypeCheckResults = typeCheckResults
                            Ast.File = file
                        } |> Success
                    | Failed(_) -> 
                        Failed(AbortedTypeCheck)
            | None -> 
                Failed(FailedToParseFile(parseResults.Errors))

    /// Parses a file using `FSharp.Compiler.Services`.
    let parseFile file configuration =
        let source = System.IO.File.ReadAllText(file)
        
        parse configuration file source
        
    /// Parses source code using `FSharp.Compiler.Services`.
    let parseSource source configuration =
        let file = "dog.fsx"
        
        parse configuration file source