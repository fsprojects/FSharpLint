module TestApp

open System
open System.IO
open Microsoft.Build.Locator
open NUnit.Framework

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
    [<OneTimeSetUp>]
    static member Setup() =
        if not MSBuildLocator.IsRegistered then
            MSBuildLocator.RegisterDefaults() |> ignore

    [<Test>]
    member __.``Lint file, expected rules are triggered.``() =
        let config = """
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """
        use input = new TemporaryFile(config, "fs")

        let (returnCode, errors) = main [| "lint"; input.FileName |]

        Assert.AreEqual(-1, returnCode)
        Assert.AreEqual(set ["Consider changing `Signature` to be prefixed with `I`."], errors)

    [<Test>]
    member __.``Lint source without any config, rule enabled in default config is triggered for given source.``() =
        let input = """
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """

        let (returnCode, errors) = main [| "lint"; input |]

        Assert.AreEqual(-1, returnCode)
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

        Assert.AreEqual(0, returnCode)
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
        
        Assert.AreEqual(0, returnCode)
        Assert.AreEqual(Set.empty, errors)
