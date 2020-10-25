module TestApp

open System
open System.IO
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

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/466
    /// Hints listed in the ignore section of the config were not being ignored.
    [<Test>]
    member __.``Ignoring a hint in the configuration should stop it from being output as warning.``() =
        let input = """
        let x = [1; 2; 3; 4] |> List.map (fun x -> x + 2) |> List.map (fun x -> x + 2)
        """
        
        let (returnCode, errors) = main [| "lint"; input |]
        
        // Check default config triggers the hint we're expecting to ignore later.
        Assert.AreEqual(-1, returnCode)
        Assert.AreEqual(Set.ofList ["`List.map f (List.map g x)` might be able to be refactored into `List.map (g >> f) x`."], errors)

        let config = """
        {
            "hints": {
                "add": [],
                "ignore": [
                    "List.map f (List.map g x) ===> List.map (g >> f) x"
                ]
            }
        }
        """
        use config = new TemporaryFile(config, "json")

        let (returnCode, errors) = main [| "lint"; "--lint-config"; config.FileName; input |]

        Assert.AreEqual(0, returnCode)
        Assert.AreEqual(Set.empty, errors)
        
    /// Regression for bug discovered during: https://github.com/fsprojects/FSharpLint/issues/466
    /// Adding a rule to the config was disabling other rules unless they're explicitly specified.
    [<Test>]
    member __.``Adding a rule to a custom config should not have side effects on other rules (from the default config).``() =
        let config = """
        {
            exceptionNames: {
                enabled: false
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
        
        Assert.AreEqual(-1, returnCode)
        Assert.AreEqual(Set.ofList ["Consider changing `Signature` to be prefixed with `I`."], errors)
