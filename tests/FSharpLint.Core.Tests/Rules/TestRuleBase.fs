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

module TestRuleBase

open System
open System.Diagnostics
open System.Text
open NUnit.Framework
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpLint.Framework
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.ParseFile
open TestUtils

let emptyConfig =
    { UseTypeChecker = Some(false)
      IgnoreFiles = Some({ Files = []; Update = IgnoreFiles.Add; Content = "" })
      Analysers =
          Map.ofList
              [ ("", { Rules = Map.ofList [ ("", { Settings = Map.ofList [ ("", Enabled(true)) ] }) ]
                       Settings = Map.ofList [] }) ] }

[<AbstractClass>]
type TestRuleBase(analyser, ?analysers) =
    let suggestions = ResizeArray<_>()

    let postSuggestion suggestion =
        suggestions.Add(suggestion)

    let config =
        match analysers with
        | Some(analysers) -> 
            { UseTypeChecker = Some(false)
              IgnoreFiles = Some({ Files = []; Update = IgnoreFiles.Add; Content = "" })
              Analysers = analysers }
        | None -> emptyConfig

    member __.TimeAnalyser(iterations, ?overrideConfig) =
        let (tree, text) = getPerformanceTestInput ()

        let (array, skipArray) = AbstractSyntaxArray.astToArray tree

        let config = match overrideConfig with Some(overrideConfig) -> overrideConfig | None -> config

        let visitorInfo =
            { FSharpVersion = System.Version(3, 1); Config = config; Suggest = ignore; Text = text }

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

        let visitorInfo = { Config = config; Suggest = postSuggestion; FSharpVersion = version; Text = input }
        
        match parseSource input config (FSharpChecker.Create()) with
        | Success(parseInfo) ->
            let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray parseInfo.Ast
            analyser visitorInfo parseInfo.TypeCheckResults syntaxArray skipArray
        | _ -> failwith "Failed to parse input."

    member __.ErrorExistsAt(startLine, startColumn) =
        suggestions
        |> Seq.exists (fun s -> s.Range.StartLine = startLine && s.Range.StartColumn = startColumn)

    member __.ErrorsAt(startLine, startColumn) =
        suggestions
        |> Seq.filter (fun s -> s.Range.StartLine = startLine && s.Range.StartColumn = startColumn)

    member __.ErrorExistsOnLine(startLine) =
        suggestions
        |> Seq.exists (fun s -> s.Range.StartLine = startLine)

    member __.NoErrorExistsOnLine(startLine) =
        suggestions
        |> Seq.exists (fun s -> s.Range.StartLine = startLine)
        |> not

    // prevent tests from passing if errors exist, just not on the line being checked
    member __.NoErrorsExist =
        suggestions |> Seq.isEmpty

    member __.ErrorsExist =
        suggestions |> Seq.isEmpty |> not

    member __.ErrorMsg =
        match suggestions with
        | xs when xs.Count = 0 -> "No errors"
        | _ ->
            suggestions
            |> Seq.map (fun s -> (sprintf "((%i, %i) - (%i, %i) -> %s)"
                s.Range.StartRange.StartLine s.Range.StartColumn 
                s.Range.EndRange.EndLine s.Range.EndRange.EndColumn s.Message ))
            |> (fun x -> String.Join("; ", x))

    member this.ErrorWithMessageExistsAt(message, startLine, startColumn) =
        this.ErrorsAt(startLine, startColumn)
        |> Seq.exists (fun s -> s.Message = message)

    member __.ErrorWithMessageExists(message) =
        suggestions |> Seq.exists (fun s -> s.Message = message)

    member this.AssertNoWarnings() =
        Assert.IsFalse(this.ErrorsExist, "Expected no errors, but was: " + this.ErrorMsg)

    member this.ApplyQuickFix (source:string) =
        let firstSuggestedFix =
            suggestions 
            |> Seq.choose (fun x -> x.SuggestedFix)
            |> Seq.tryHead

        match firstSuggestedFix with
        | Some(fix) ->
            let startIndex = ExpressionUtilities.findPos fix.FromRange.Start source
            let endIndex = ExpressionUtilities.findPos fix.FromRange.End source

            match startIndex, endIndex with
            | Some(startIndex), Some(endIndex) -> 
                (StringBuilder source)
                    .Remove(startIndex, endIndex - startIndex)
                    .Insert(startIndex, fix.ToText)
                    .ToString()
            | _ -> source
        | None -> source

    [<SetUp>]
    member __.SetUp() = suggestions.Clear()