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

module TestRuleBase

open System.Diagnostics
open NUnit.Framework
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpLint.Framework
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.ParseFile

let emptyConfig =
    { UseTypeChecker = Some(false)
      IgnoreFiles = Some({ Files = []; Update = IgnoreFiles.Add; Content = "" })
      Analysers =
          Map.ofList
              [ ("", { Rules = Map.ofList [ ("", { Settings = Map.ofList [ ("", Enabled(true)) ] }) ]
                       Settings = Map.ofList [] }) ] }

[<Literal>]
let SourceFile = "../../../FSharpLint.Framework.Tests/TypeChecker.fs"

let generateAst source =
    let checker = FSharpChecker.Create()

    let options = 
        checker.GetProjectOptionsFromScript(SourceFile, source) 
        |> Async.RunSynchronously

    let parseResults =
        checker.ParseFileInProject(SourceFile, source, options)
        |> Async.RunSynchronously
        
    match parseResults.ParseTree with
    | Some(parseTree) -> parseTree
    | None -> failwith "Failed to parse file."

[<AbstractClass>]
type TestRuleBase(analyser, ?analysers) =
    let errorRanges = System.Collections.Generic.List<range * string>()

    let postError (range:range) error =
        errorRanges.Add(range, error)

    let config =
        match analysers with
        | Some(analysers) -> 
            { UseTypeChecker = Some(false)
              IgnoreFiles = Some({ Files = []; Update = IgnoreFiles.Add; Content = "" })
              Analysers = analysers }
        | None -> emptyConfig

    member __.TimeAnalyser(iterations, ?overrideConfig) =
        let text = System.IO.File.ReadAllText SourceFile
        let tree = text |> generateAst

        let (array, skipArray) = AbstractSyntaxArray.astToArray tree

        let config = match overrideConfig with Some(overrideConfig) -> overrideConfig | None -> config

        let visitorInfo =
            { FSharpVersion = System.Version(); Config = config; PostError = (fun _ _ -> ()); Text = text }

        let stopwatch = Stopwatch.StartNew()
        let times = ResizeArray()

        for _ in 0..iterations do
            stopwatch.Restart()

            analyser 
                visitorInfo
                None
                array
                skipArray

            stopwatch.Stop()

            times.Add stopwatch.ElapsedMilliseconds

        let result = times |> Seq.sum |> (fun totalMilliseconds -> totalMilliseconds / int64 iterations)

        System.Console.WriteLine(sprintf "Average runtime of analyser: %d (milliseconds)."  result)

        result

    member __.Parse(input:string, ?overrideAnalysers, ?checkInput, ?fsharpVersion): unit = 
        let config =
            match overrideAnalysers with
            | Some(overrideAnalysers) -> 
                { UseTypeChecker = Some(false)
                  IgnoreFiles = Some({ Files = []; Update = IgnoreFiles.Add; Content = "" })
                  Analysers = overrideAnalysers }
            | None -> config

        let checkInput = match checkInput with | Some(x) -> x | None -> false

        let config = { config with UseTypeChecker = Some(checkInput) }

        let version = match fsharpVersion with | Some(x) -> x | None -> System.Version(4, 0)

        let visitorInfo = { Config = config; PostError = postError; FSharpVersion = version; Text = input }
        
        match parseSource input config (FSharpChecker.Create()) with
        | Success(parseInfo) ->
            let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray parseInfo.Ast
            analyser visitorInfo parseInfo.TypeCheckResults syntaxArray skipArray
        | _ -> failwith "Failed to parse input."

    member __.ErrorExistsAt(startLine, startColumn) =
        errorRanges
        |> Seq.exists (fun (r, _) -> r.StartLine = startLine && r.StartColumn = startColumn)

    member __.ErrorsAt(startLine, startColumn) =
        errorRanges
        |> Seq.filter (fun (r, _) -> r.StartLine = startLine && r.StartColumn = startColumn)

    member __.ErrorExistsOnLine(startLine) =
        errorRanges
        |> Seq.exists (fun (r, _) -> r.StartLine = startLine)

    member __.NoErrorExistsOnLine(startLine) =
        errorRanges
        |> Seq.exists (fun (r, _) -> r.StartLine = startLine)
        |> not

    // prevent tests from passing if errors exist, just not on the line being checked
    member __.NoErrorsExist =
        errorRanges
        |> Seq.isEmpty

    member __.ErrorsExist =
        errorRanges
        |> Seq.isEmpty |> not

    member __.ErrorMsg =
        match errorRanges with
        | xs when xs.Count = 0 -> "No errors"
        | _ ->
            errorRanges
            |> Seq.map (fun (r, err) -> (sprintf "((%i, %i) - (%i, %i) -> %s)"
                r.StartRange.StartLine r.StartColumn r.EndRange.EndLine r.EndRange.EndColumn err ))
            |> (fun x -> System.String.Join("; ", x))

    member this.ErrorWithMessageExistsAt(message, startLine, startColumn) =
        this.ErrorsAt(startLine, startColumn)
        |> Seq.exists (fun (_, e) -> e = message)

    member __.ErrorWithMessageExists(message) =
        errorRanges |> Seq.exists (fun (_, e) -> e = message)

    member this.AssertNoWarnings() =
        Assert.IsFalse(this.ErrorsExist, "Expected no errors, but was: " + this.ErrorMsg)

    [<SetUp>]
    member __.SetUp() = errorRanges.Clear()