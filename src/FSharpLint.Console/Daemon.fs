module FSharpLint.Console.Daemon

open System
open System.Diagnostics
open System.IO
open System.Threading
open StreamJsonRpc
open FSharp.Core
open FSharpLint.Application
open Newtonsoft.Json
open FSharpLint.Client.Contracts

let private toClientLintWarning (lintWarning: FSharpLint.Framework.Suggestion.LintWarning): ClientLintWarning =
    {
        ErrorText = lintWarning.ErrorText
        FilePath = lintWarning.FilePath
        RuleIdentifier = lintWarning.RuleIdentifier
        RuleName = lintWarning.RuleName
        Details = {
            Range = ClientRange(lintWarning.Details.Range.StartLine, lintWarning.Details.Range.StartColumn, lintWarning.Details.Range.EndLine, lintWarning.Details.Range.EndColumn)
            Message = lintWarning.Details.Message
            SuggestedFix = 
                lintWarning.Details.SuggestedFix
                |> Option.bind(fun fix -> fix.Value)
                |> Option.map(fun fix -> {
                    FromRange = ClientRange(fix.FromRange.StartLine, fix.FromRange.StartColumn, fix.FromRange.EndLine, fix.FromRange.EndColumn)
                    FromText = fix.FromText
                    ToText = fix.ToText
                })
        }
    }

type FSharpLintDaemon(sender: Stream, reader: Stream) as this =
    let rpc: JsonRpc = JsonRpc.Attach(sender, reader, this)
    let traceListener = new DefaultTraceListener()

    do
        // hook up request/response logging for debugging
        rpc.TraceSource <- TraceSource(typeof<FSharpLintDaemon>.Name, SourceLevels.Verbose)
        rpc.TraceSource.Listeners.Add traceListener |> ignore<int>

    let disconnectEvent = new ManualResetEvent(false)

    let exit () = disconnectEvent.Set() |> ignore<bool>

    do rpc.Disconnected.Add(fun _ -> exit ())

    interface IDisposable with
        member this.Dispose() =
            traceListener.Dispose()
            disconnectEvent.Dispose()

    /// returns a hot task that resolves when the stream has terminated
    member this.WaitForClose = rpc.Completion

    [<JsonRpcMethod(Methods.Version)>]
    member _.Version() : string = FSharpLint.Console.Version.get ()

    [<JsonRpcMethod(Methods.LintFile)>]
    member _.LintFile(request: LintFileRequest) : Result<ClientLintWarning list, string> =
        let lintConfig = 
            match request.LintConfigPath with
            | Some path -> 
                { CancellationToken = None
                  ReceivedWarning = None
                  Configuration = FromFile path
                  ReportLinterProgress = None }
            | None -> Lint.OptionalLintParameters.Default

        let lintResult = Lint.lintFile lintConfig (request.FilePath)
        match lintResult with
        | LintResult.Success warnings ->
            let result = warnings |> List.map toClientLintWarning
            Debug.Assert (JsonConvert.SerializeObject result <> "")

            Ok result
        | LintResult.Failure failure -> Error failure.Description
