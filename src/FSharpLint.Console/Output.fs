module FSharpLint.Console.Output

open System
open FSharp.Compiler.Range
open FSharpLint.Application.Lint
open FSharpLint.Framework

type IOutput =
    abstract member HandleLintEvent : LintEvent -> unit
    abstract member HandleLintResult : Result<Suggestion.LintWarning list, LintFailure> -> unit
    abstract member HandleException : string -> unit

let private getParseFailureReason = function
    | ParseFile.FailedToParseFile failures ->
        let getFailureReason (x:FSharp.Compiler.SourceCodeServices.FSharpErrorInfo) =
            sprintf "failed to parse file %s, message: %s" x.FileName x.Message

        String.Join(", ", failures |> Array.map getFailureReason)
    | ParseFile.AbortedTypeCheck -> "Aborted type check."

[<RequireQualifiedAccess>]
type Standard () =

    let getErrorMessage (range:FSharp.Compiler.Range.range) =
        Resources.Format("LintSourceError", range.StartLine, range.StartColumn)

    let highlightErrorText (range:range) (errorLine:string) =
        let highlightColumnLine =
            if String.length errorLine = 0 then "^"
            else
                errorLine
                |> Seq.mapi (fun i _ -> if i = range.StartColumn then "^" else " ")
                |> Seq.reduce (+)
        getErrorMessage range + Environment.NewLine + errorLine + Environment.NewLine + highlightColumnLine

    let writeLine (str:string) (color:ConsoleColor) (writer:IO.TextWriter) =
        let originalColour = Console.ForegroundColor
        Console.ForegroundColor <- color
        writer.WriteLine str
        Console.ForegroundColor <- originalColour

    let writeInfo (info:string) = writeLine info ConsoleColor.White Console.Out

    let writeWarning (warning:Suggestion.LintWarning) =
        let highlightedErrorText = highlightErrorText warning.Details.Range warning.ErrorText
        let str = warning.Details.Message + Environment.NewLine + highlightedErrorText
        writeLine str ConsoleColor.Yellow Console.Out
        String.replicate 80 "-" |> writeInfo

    let writeError (error:string) =  writeLine error ConsoleColor.Red Console.Error

    interface IOutput with

        member __.HandleLintEvent (event : LintEvent) =
            match event with
            | LintEvent.FinishedLintingFile (fileName, warnings) ->
                if not (List.isEmpty warnings) then
                    Resources.Format("ConsoleStartingFile", fileName) |> writeInfo
                    warnings |> List.iter writeWarning
                    Resources.Format("ConsoleFinishedFile", fileName, List.length warnings) |> writeInfo
            | _ -> ()

        member __.HandleLintResult (result : Result<Suggestion.LintWarning list, LintFailure>) =
            match result with
            | Ok warnings ->
                Resources.Format("ConsoleFinished", List.length warnings) |> writeInfo
            | Error (FailedToLoadFile filePath) ->
                Resources.Format("ConsoleCouldNotFindFile", filePath) |> writeError
            | Error (RunTimeConfigError error) ->
                Resources.Format("ConsoleRunTimeConfigError", error) |> writeError
            | Error (FailedToParseFiles failures) ->
                let failureReasons = failures |> List.map getParseFailureReason |> String.concat "\n"
                "Lint failed to parse files. Failed with: " + failureReasons |> writeError

        member __.HandleException (message : string) =
            message |> Console.Error.WriteLine

[<RequireQualifiedAccess>]
type MSBuild () =

    interface IOutput with
        member __.HandleLintEvent (event : LintEvent) =
            match event with
            | LintEvent.ReceivedWarning warning ->
                sprintf "%s(%d,%d,%d,%d):FSharpLint warning %s: %s"
                    <| warning.FilePath
                    <| warning.Details.Range.StartLine
                    <| warning.Details.Range.StartColumn
                    <| warning.Details.Range.EndLine
                    <| warning.Details.Range.EndColumn
                    <| warning.RuleIdentifier
                    <| warning.Details.Message
                |> Console.Out.WriteLine
            | _ -> ()

            member __.HandleLintResult (_ : Result<Suggestion.LintWarning list, LintFailure>) = ()

            member __.HandleException (message : string) =
                sprintf "FSharpLint error: %s" message |> Console.Error.WriteLine
