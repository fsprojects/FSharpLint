module TestApp

open System
open System.IO
open NUnit.Framework
open FSharpLint.Console.Program

let getErrorsFromOutput (output:string) =
    let splitOutput = output.Split([|Environment.NewLine|], StringSplitOptions.None)

    set [ for i in 1..splitOutput.Length - 1 do
            if splitOutput.[i].StartsWith "Error" then yield splitOutput.[i - 1] ]

type TemporaryFile(config, extension) =
    let filename = Path.ChangeExtension(Path.GetTempFileName(), extension)
    do
        File.WriteAllText(filename, config)

    member __.FileName = filename

    interface System.IDisposable with
        member __.Dispose() =
            File.Delete(filename)

let main input =
    use stdout = new StringWriter()
    let existing = Console.Out
    Console.SetOut(stdout)
    try
        let returnCode = FSharpLint.Console.Program.main input
        (returnCode, getErrorsFromOutput <| stdout.ToString())
    finally
        Console.SetOut(existing)

[<TestFixture>]
type TestConsoleApplication() =
    [<Test>]
    member __.``Lint file, expected rules are triggered.``() =
        let config = """
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """
        use input = new TemporaryFile(config, "fs")

        let (returnCode, errors) = main [| "lint"; input.FileName |]

        Assert.AreEqual(int ExitCode.Error, returnCode)
        Assert.AreEqual(set ["Consider changing `Signature` to be prefixed with `I`."], errors)

    [<Test>]
    member __.``Lint source without any config, rule enabled in default config is triggered for given source.``() =
        let input = """
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """

        let (returnCode, errors) = main [| "lint"; input |]

        Assert.AreEqual(int ExitCode.Error, returnCode)
        Assert.AreEqual(set ["Consider changing `Signature` to be prefixed with `I`."], errors)

    [<Test>]
    member __.``Lint source with valid config to disable rule, disabled rule is not triggered for given source.``() =
        let config = """
        {
            "InterfaceNames": {
                "enabled": false
            }
        }
        """
        use config = new TemporaryFile(config, "json")

        let input = """
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """

        let (returnCode, errors) = main [| "lint"; "--lint-config"; config.FileName; input |]

        Assert.AreEqual(int ExitCode.Success, returnCode)
        Assert.AreEqual(Set.empty, errors)
        
    [<Test>]
    member __.``Lint source with error suppressed, no error is given.``() =
        let input = """
        // fsharplint:disable-next-line
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """
        
        let (returnCode, errors) = main [| "lint"; input |]
        
        Assert.AreEqual(int ExitCode.Success, returnCode)
        Assert.AreEqual(Set.empty, errors)

    [<Test>]
    member __.``Lint source with fix option``() =
        let sourceCode = """
module Fass =
    let foo = new System.Collections.Generic.Dictionary<string, string>() |> ignore
    let goo = new Guid() |> ignore
    let ntoo = new Int32() |> ignore
module Fall =
    let uoo = new Uid() |> ignore
    let version =  new System.Version()
    let xoo = new Uint32() |> ignore
        """

        let expected = """
module Fass =
    let foo = System.Collections.Generic.Dictionary<string, string>() |> ignore
    let goo = Guid() |> ignore
    let ntoo = Int32() |> ignore
module Fall =
    let uoo = Uid() |> ignore
    let version =  System.Version()
    let xoo = Uint32() |> ignore
        """
        let ruleName = "RedundantNewKeyword"
        use input = new TemporaryFile(sourceCode, "fs")
        let (exitCode, errors) = main [| "fix"; ruleName; input.FileName |]

        Assert.AreEqual(int ExitCode.Success, exitCode)
        Assert.AreEqual(set ["Usage of `new` keyword here is redundant."], errors)
        Assert.AreEqual(expected, File.ReadAllText input.FileName)

    [<Test>]
    member __.``Lint source with fix option with wrong rulename``() =
        let sourceCode = """
printfn "Hello"
        """

        let ruleName = "ssrffss"
        use input = new TemporaryFile(sourceCode, "fs")
        let (exitCode, errors) = main [| "fix"; ruleName; input.FileName |]

        Assert.AreEqual(int ExitCode.NoSuchRuleName, exitCode)

    [<Test>]
    member __.``Lint source with fix option no need for fix``() =
        let sourceCode = """
module Fass =
    let foo = System.Collections.Generic.Dictionary<string, string>() |> ignore
    let goo = Guid() |> ignore
    let ntoo = Int32() |> ignore
module Fall =
    let uoo = Uid() |> ignore
    let version =  System.Version()
    let xoo = Uint32() |> ignore
        """
        let ruleName = "RedundantNewKeyword"
        use input = new TemporaryFile(sourceCode, "fs")
        let (exitCode, errors) = main [| "fix"; ruleName; input.FileName |]

        Assert.AreEqual(int ExitCode.NoSuggestedFix, exitCode)
        Assert.AreEqual(sourceCode, File.ReadAllText input.FileName)
