#r "nuget: Fake.DotNet.Cli"
#r "nuget: Fake.Core.Process"
#r "nuget: Fake.Core.ReleaseNotes"
#load "build.fsx"

open Fake.DotNet

let filterPerformanceTests (p:DotNet.TestOptions) = { p with Filter = Some "\"TestCategory!=Performance\""; Configuration = DotNet.Release }

DotNet.test filterPerformanceTests "tests/FSharpLint.Core.Tests"
DotNet.test filterPerformanceTests "tests/FSharpLint.Console.Tests"
DotNet.restore id "tests/FSharpLint.FunctionalTest.TestedProject/FSharpLint.FunctionalTest.TestedProject.sln"
DotNet.test filterPerformanceTests "tests/FSharpLint.FunctionalTest"
