module FSharpLint.Console.Output

open System
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Violation

type IOutput =
    /// Outputs informational text.
    abstract member WriteInfo : string -> unit
    /// Outputs a lint rule violation.
    abstract member WriteViolationInfo : LintViolation -> unit
    /// Outputs an unexpected failure when linting.
    abstract member WriteFailure : string -> unit

type StandardOutput () =

    let getRangeInfoMessage (range:Range) =
        let msg = Resources.GetString "LintSourceViolation"
        String.Format(msg, range.StartLine, range.StartColumn)

    let highlightViolationText (range:Range) (sourceLineWithViolation:string) =
        let highlightColumnLine =
            if String.length sourceLineWithViolation = 0 then "^"
            else
                sourceLineWithViolation
                |> Seq.mapi (fun index _ -> if index = range.StartColumn then "^" else " ")
                |> Seq.reduce (+)
        $"{getRangeInfoMessage range}{Environment.NewLine}{sourceLineWithViolation}{Environment.NewLine}{highlightColumnLine}"

    let writeLine (str:string) (color:ConsoleColor) (writer:IO.TextWriter) =
        let originalColour = Console.ForegroundColor
        Console.ForegroundColor <- color
        writer.WriteLine str
        Console.ForegroundColor <- originalColour

    interface IOutput with

        member _.WriteInfo (info:string) = writeLine info ConsoleColor.White Console.Out
        member this.WriteViolationInfo (violation: LintViolation) =
            let highlightedViolatonText = highlightViolationText violation.Details.Range violation.SourceCodeFragment
            let ruleUrlHint = $"See https://fsprojects.github.io/FSharpLint/how-tos/rules/%s{violation.RuleIdentifier}.html"
            let str = $"{violation.Details.Message}{Environment.NewLine}{highlightedViolatonText}{Environment.NewLine}{ruleUrlHint}"
            writeLine str ConsoleColor.Yellow Console.Out
            String.replicate 80 "-" |> (this :> IOutput).WriteInfo
        member _.WriteFailure (failure: string) =  writeLine failure ConsoleColor.Red Console.Error

type MSBuildOutput () =

    interface IOutput with

        member _.WriteInfo (info:string) = Console.Out.WriteLine info
        member _.WriteViolationInfo (violation: LintViolation) =
            fprintf Console.Out "%s(%d,%d,%d,%d):FSharpLint violation %s: %s"
                <| violation.FilePath
                <| violation.Details.Range.StartLine
                <| violation.Details.Range.StartColumn
                <| violation.Details.Range.EndLine
                <| violation.Details.Range.EndColumn
                <| violation.RuleIdentifier
                <| violation.Details.Message
        member _.WriteFailure (failure: string) =
            Console.Error.WriteLine $"FSharpLint failure: {failure}"
