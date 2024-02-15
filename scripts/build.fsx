#r "nuget: Fake.DotNet.Cli"
#load "clean.fsx"

open Fake.DotNet

DotNet.build id "FSharpLint.sln"
