module FSharpLint.Console.Daemon

open System
open System.Diagnostics
open System.IO
open System.Threading
open StreamJsonRpc
open FSharpLint.Client.Contracts
open FSharp.Core

type FSharpLintDaemon(sender: Stream, reader: Stream) as this =
    let rpc: JsonRpc = JsonRpc.Attach(sender, reader, this)
    let traceListener = new DefaultTraceListener()

    do
        // hook up request/response logging for debugging
        rpc.TraceSource <- TraceSource(typeof<FSharpLintDaemon>.Name, SourceLevels.Verbose)
        rpc.TraceSource.Listeners.Add traceListener |> ignore<int>

    let disconnectEvent = new ManualResetEvent(false)

    let exit () = disconnectEvent.Set() |> ignore

    do rpc.Disconnected.Add(fun _ -> exit ())

    interface IDisposable with
        member this.Dispose() =
            traceListener.Dispose()
            disconnectEvent.Dispose()

    /// returns a hot task that resolves when the stream has terminated
    member this.WaitForClose = rpc.Completion

    [<JsonRpcMethod(Methods.Version)>]
    member _.Version() : string = FSharpLint.Console.Version.get ()
