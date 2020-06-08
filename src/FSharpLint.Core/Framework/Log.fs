module FSharpLint.Framework.Log

open System
open System.Diagnostics

let mutable private logHandler = None

let internal setHandler handler =
    logHandler <- handler

let internal info format =
    Printf.kprintf (fun s -> logHandler |> Option.iter (fun handler -> handler (sprintf "%s" s, false))) format

let internal error format =
    Printf.kprintf (fun s -> logHandler |> Option.iter (fun handler -> handler (sprintf "%s" s, true))) format

type internal TimeLogger (logF:string -> unit, message:string) =
    let sw = Stopwatch.StartNew()

    interface IDisposable with
        member __.Dispose () =
            sw.Stop()
            logF (sprintf "%s|elapsedMs=%f" message sw.Elapsed.TotalMilliseconds)

