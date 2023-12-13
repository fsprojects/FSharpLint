open BenchmarkDotNet.Configs
open BenchmarkDotNet.Diagnostics.Windows
open BenchmarkDotNet.Environments
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Running
open FSharpLint.Benchmarks

[<EntryPoint>]
let main _ =
    BenchmarkRunner
        .Run<Benchmark>(
            DefaultConfig.Instance
                .AddJob(Job.Default)
                .AddDiagnoser(EtwProfiler())) |> ignore<BenchmarkDotNet.Reports.Summary>
    0
