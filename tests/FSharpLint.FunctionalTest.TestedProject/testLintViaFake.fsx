#r @"../../packages/FAKE/tools/FakeLib.dll"
open Fake
open System

#I @"FSharpLint/"
#r @"FSharpLint/FSharpLint.FAKE.dll"
open FSharpLint.FAKE

Target "Lint" (fun _ -> !! "FSharpLint.FunctionalTest.TestedProject.fsproj" |> Seq.iter (FSharpLint id))

RunTargetOrDefault "Lint"