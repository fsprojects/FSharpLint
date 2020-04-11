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
          
    [<TestFixture>]
    type TestConsoleApplication() =
        [<Test>]
        member __.``Lint source without any config, rule enabled in default config is triggered for given source.``() =
            use stdout = new StringWriter()
            Console.SetOut(stdout)

            let input = """
            type Signature =
                abstract member Encoded : string
                abstract member PathName : string
                        """

            let returnCode = FSharpLint.Console.Program.main [| "lint"; input |]
            
            Assert.AreEqual(-1, returnCode)
            
            let errors = getErrorsFromOutput <| stdout.ToString()
            Assert.AreEqual(set ["Consider changing `Signature` to be prefixed with `I`."], errors)
            
        [<Test>]
        member __.``Lint source with valid config to disable rule, disabled rule is not triggered for given source.``() =
            use stdout = new StringWriter()
            Console.SetOut(stdout)

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

            let returnCode = FSharpLint.Console.Program.main [| "lint"; "--lint-config"; config.FileName; input |]
            Assert.AreEqual(0, returnCode)
