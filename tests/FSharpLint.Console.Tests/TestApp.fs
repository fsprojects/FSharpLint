namespace FSharpLint.FunctionalTest

module Tests =

    open System
    open System.IO
    open NUnit.Framework

    let getErrorsFromOutput (output:string) =
        let splitOutput = output.Split([|Environment.NewLine|], StringSplitOptions.None)

        set [ for i in 1..splitOutput.Length - 1 do
                if splitOutput.[i].StartsWith "Error" then yield splitOutput.[i - 1] ]

    type TemporaryConfig(config) = 
        let filename = Path.GetTempFileName()
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
        let returnCode = FSharpLint.Console.Program.main input
        Console.SetOut(existing)
        (returnCode, getErrorsFromOutput <| stdout.ToString())
          
    [<TestFixture>]
    type TestConsoleApplication() =
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
            use config = new TemporaryConfig("""
            {
                "InterfaceNames": {
                    "enabled": false
                }
            }
            """)

            let input = """
            type Signature =
                abstract member Encoded : string
                abstract member PathName : string
            """

            let (returnCode, errors) = main [| "lint"; "--lint-config"; config.FileName; input |]
            
            Assert.AreEqual(0, returnCode)
            Assert.AreEqual(set [], errors)
