namespace FSharpLint.FunctionalTest

module Tests =

    open FSharpLint.Application
    open FSharpLint.Framework
    open System
    open System.Diagnostics
    open System.IO
    open NUnit.Framework

    let (</>) x y = Path.Combine(x, y)

    let basePath = TestContext.CurrentContext.TestDirectory </> ".." </> ".." </> ".." </> ".." </> ".."

    type Error =
        { Description: string
          Location: string
          Code: string }

        override this.ToString() =
            sprintf "{\n    Description=\"%s\"\n    Location=\"%s\"\n    Code=\"%s\"\n}" this.Description this.Location this.Code

    let dotnetFslint arguments =        
        let binDir = 
            #if DEBUG
                "Debug"
            #else
                "Release"
            #endif

        let dll = basePath </> "src" </> "FSharpLint.Console" </> "bin" </> binDir </> "netcoreapp2.1" </> "dotnet-fsharplint.dll"

        let startInfo = ProcessStartInfo
                                (FileName = "dotnet",
                                 Arguments = dll + " " + arguments,
                                 RedirectStandardOutput = true,
                                 RedirectStandardError = true,
                                 UseShellExecute = false,
                                 WorkingDirectory = (basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject"))

        use app = Process.Start(startInfo)
        let output = app.StandardOutput.ReadToEnd()
        let output = output + app.StandardError.ReadToEnd()
        app.WaitForExit()
        output

    let getErrorsFromOutput (output:string) = 
        let splitOutput = output.Split([|Environment.NewLine|], StringSplitOptions.None)
        
        set [ for i in 1..splitOutput.Length - 1 do
                if splitOutput.[i].StartsWith "Error" then yield splitOutput.[i - 1] ]

    let expectedErrors =
        set [ 
          "FL0065: `not (a = b)` might be able to be refactored into `a <> b`."
          "FL0065: `not (a <> b)` might be able to be refactored into `a = b`."
          "FL0065: `fun x -> x` might be able to be refactored into `id`."
          "FL0065: `not true` might be able to be refactored into `false`."
          "FL0065: `not false` might be able to be refactored into `true`."
          "FL0065: `List.fold ( + ) 0 x` might be able to be refactored into `List.sum x`."
          "FL0065: `a <> true` might be able to be refactored into `not a`."
          "FL0065: `x = null` might be able to be refactored into `isNull x`."
          "FL0065: `List.head (List.sort x)` might be able to be refactored into `List.min x`." ]
        
    [<TestFixture(Category = "Acceptance Tests")>]
    type TestConsoleApplication() =
        let projectPath =
            basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject"

        [<Test>]
        member __.InvalidConfig() = 
            let projectFile = projectPath </> "FSharpLint.FunctionalTest.TestedProject.fsproj"
            let arguments = sprintf "-f %s" projectFile

            File.WriteAllText(projectPath </> "fsharplint.json", "invalid config file contents")

            let output = dotnetFslint arguments

            File.Delete(projectPath </> "fsharplint.json")

            Assert.IsTrue(output.Contains("Failed to load config file"), sprintf "Output:\n%s" output)

        [<Test>]
        member __.UnableToFindProjectFile() = 
            let projectFile = projectPath </> "iuniubi.fsproj"
            let arguments = sprintf "-f %s" projectFile

            let output = dotnetFslint arguments

            Assert.IsTrue(
                output.Contains(sprintf "Could not find the file: %s on disk" projectFile), 
                sprintf "Output:\n%s" output)

        [<Test>]
        member __.FunctionalTestConsoleApplication() = 
            let projectFile = projectPath </> "FSharpLint.FunctionalTest.TestedProject.fsproj"
            let arguments = sprintf "-f %s" projectFile

            let output = dotnetFslint arguments
            let errors = getErrorsFromOutput output
            
            let expectedMissing = Set.difference expectedErrors errors
            let notExpected = Set.difference errors expectedErrors

            Assert.AreEqual(expectedErrors, errors, 
                "Did not find the following expected errors: [" + String.concat "," expectedMissing + "]\n" + 
                "Found the following unexpected warnings: [" + String.concat "," notExpected + "]")
            
        [<Test>]
        member __.FunctionalTestConfigConversion() =
            let xmlFile = TestContext.CurrentContext.TestDirectory </> "OldConfiguration.xml"
            let outputFile = TestContext.CurrentContext.TestDirectory </> "convertedConfig.json"
            let arguments = sprintf "-convert %s %s" xmlFile outputFile

            let output = dotnetFslint arguments
            
            let expectedOutput = sprintf "Successfully converted config at '%s', saved to '%s'" xmlFile outputFile

            // Check dotnet tool output.
            Assert.AreEqual(expectedOutput.Trim(), output.Trim())
            
            // Check converted config contents.
            let convertedConfig =
                File.ReadAllText outputFile
                |>  ConfigurationManager.parseConfig
                
            let expectedConfig =
                { Configuration.defaultConfiguration with hints = Configuration.defaultConfiguration.hints |> Option.map (fun hintsConfig -> { hintsConfig with ignore = None }) }
                
            Assert.AreEqual(expectedConfig, convertedConfig)
