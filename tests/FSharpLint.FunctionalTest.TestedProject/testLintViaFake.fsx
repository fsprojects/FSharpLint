#r @"../../packages/tools/FAKE/tools/FakeLib.dll"
open Fake
open System

#I @"FSharpLintFakeTaskTest/"
#r @"FSharpLintFakeTaskTest/FSharpLint.Fake.dll"
open FSharpLint.Fake

Target "Lint" (fun _ -> !! "FSharpLint.FunctionalTest.TestedProject.fsproj" |> Seq.iter (FSharpLint id))

RunTargetOrDefault "Lint"