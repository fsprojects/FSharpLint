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
                .AddJob(Job.Default.WithRuntime(CoreRuntime.Core21))
                .AddDiagnoser(EtwProfiler())) |> ignore
    0
