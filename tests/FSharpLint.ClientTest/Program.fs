open System
open FSharpLint.Client.LSPFSharpLintService
open FSharpLint.Client.Contracts
open System.Threading
open System.Diagnostics

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

Environment.SetEnvironmentVariable(
    "FSHARPLINT_SEARCH_PATH_OVERRIDE",
    "/home/vince/src/github/mrluje/FSharpLint.worktrees/rw/make_it_all_6/src/FSharpLint.Console/bin/Release/net6.0")

let lintActivitySource = new ActivitySource("fslint.client", "1.0.0")
let listener = new ActivityListener()
listener.ShouldListenTo <- fun a -> a.Name = "fslint.client"
listener.SampleUsingParentId <- fun s -> ActivitySamplingResult.AllData
listener.Sample <- fun s -> ActivitySamplingResult.AllData
ActivitySource.AddActivityListener(listener)

let fsharpLintService: FSharpLintService = new LSPFSharpLintService(lintActivitySource) :> FSharpLintService
let ct = CancellationToken()
let req = {
    FilePath = "/home/vince/src/github/mrluje/FSharpLint.worktrees/rw/make_it_all_6/tests/FSharpLint.FunctionalTest.TestedProject/FSharpLint.FunctionalTest.TestedProject.NetCore/TestHints.fs"
    LintConfigPath = None
}
let lintResponse = 
    async {
        let cts = new CancellationTokenSource(40000)
        // cts.CancelAfter(TimeSpan.FromSeconds 4)
        let! r = fsharpLintService.LintFileAsync(req, cts.Token) |> Async.AwaitTask
        return r
    }
    |> Async.RunSynchronously

let req2 = {
    FilePath = "/home/vince/src/github/mrluje/FSharpLint.worktrees/rw/make_it_all_6/tests/FSharpLint.FunctionalTest.TestedProject/FSharpLint.FunctionalTest.TestedProject.MultiTarget/TestHints.fs"
    LintConfigPath = None
}
let lintResponse2 = 
    async {
        let cts = new CancellationTokenSource(40000)
        // cts.CancelAfter(TimeSpan.FromSeconds 4)
        let! r = fsharpLintService.LintFileAsync(req2, cts.Token) |> Async.AwaitTask
        return r
    }
    |> Async.RunSynchronously
