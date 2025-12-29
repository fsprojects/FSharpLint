module FSharpLint.Client.ReferenceTests

open NUnit.Framework
open System.IO
open System
open System.Runtime.Remoting

[<Test>]
let ``FSharpLint.Client should not reference FSharpLint.Core``() =
    try
        System.Activator.CreateInstanceFrom("FSharp.Compiler.Service.dll", "FSharp.Compiler.CodeAnalysis.FSharpCheckFileResults")
        |> ignore<ObjectHandle>
    with
    | :? FileNotFoundException as e -> () // dll is missing, what we want
    | :? MissingMethodException as e -> Assert.Fail() // ctor is missing, dll was found
