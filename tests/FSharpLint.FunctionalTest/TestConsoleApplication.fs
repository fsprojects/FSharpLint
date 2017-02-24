namespace FSharpLint.FunctionalTest

module Tests =

    open System
    open System.Diagnostics
    open System.IO
    open NUnit.Framework
    open TestPackageHelper

    type Error =
        { Description: string
          Location: string
          Code: string }

        override this.ToString() =
            sprintf "{\n    Description=\"%s\"\n    Location=\"%s\"\n    Code=\"%s\"\n}" this.Description this.Location this.Code

    let runConsoleApp arguments =
        let filename = 
            #if DEBUG
                basePath </> "src" </> "FSharpLint.Console" </> "bin" </> "fsharplint.exe"
            #else
                basePath </> "bin" </> "fsharplint.exe"
            #endif

        let startInfo = ProcessStartInfo
                                (FileName = Path.GetFullPath filename,
                                 Arguments = arguments,
                                 RedirectStandardOutput = true,
                                 UseShellExecute = false)

        use app = Process.Start(startInfo)

        let output = app.StandardOutput.ReadToEnd()
                
        app.WaitForExit()

        output

    let getErrorsFromOutput (output:string) = 
        let splitOutput = output.Split([|Environment.NewLine|], StringSplitOptions.None)

        let errorIndexes = seq { for i in 0..splitOutput.Length / 4 - 1 -> 4 * i }

        [ for i in errorIndexes -> 
            { Description = splitOutput.[i]
              Location = splitOutput.[i + 1]
              Code = splitOutput.[i + 2] } ]

    let expectedErrors =
        [ "`not (a = b)` might be able to be refactored into `a <> b`."
          "`not (a <> b)` might be able to be refactored into `a = b`."
          "`fun x -> x` might be able to be refactored into `id`."
          "`not true` might be able to be refactored into `false`."
          "`not false` might be able to be refactored into `true`."
          "`List.fold ( + ) 0 x` might be able to be refactored into `List.sum x`."
          "`a <> true` might be able to be refactored into `not a`."
          "`x = null`; suggestion: Consider using pattern matching, or if you're using F# 4 then `isNull`."
          "`List.head (List.sort x)` might be able to be refactored into `List.min x`." ]
        
    [<TestFixture(Category = "Acceptance Tests")>]
    type TestConsoleApplication() =
        let getTestFilePath fileName =
            basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> fileName

        [<Test>]
        member __.InvalidConfig() = 
            let projectFile = getTestFilePath "FSharpLint.FunctionalTest.TestedProject.fsproj"
            let arguments = sprintf "-f %s" projectFile

            File.WriteAllText(getTestFilePath "Settings.FSharpLint", "invalid config file contents")

            let output = runConsoleApp arguments

            File.Delete(getTestFilePath "Settings.FSharpLint")

            Assert.IsTrue(output.Contains("Failed to load config file"), sprintf "Output:\n%s" output)

        [<Test>]
        member __.FunctionsAsExpectedWithInvalidReferencedProjectFile() = 
            let projectFile = getTestFilePath "referencesInvalidProject.fsproj"
            let arguments = sprintf "-f %s" projectFile

            let output = runConsoleApp arguments

            Assert.IsTrue(
                output.Contains("could not load the project file"), 
                "Did not find could not load project file error. Program output:\n" + output)

        [<Test>]
        member __.InvalidProjectFile() = 
            let projectFile = getTestFilePath "invalidProjectFile.fsproj"
            let arguments = sprintf "-f %s" projectFile

            let output = runConsoleApp arguments
            
            Assert.IsTrue(
                output.StartsWith("MSBuild could not load the project file") && output.Contains("invalidProjectFile.fsproj"), 
                sprintf "Output:\n%s" output)

        [<Test>]
        member __.UnableToFindProjectFile() = 
            let projectFile = getTestFilePath "iuniubi.fsproj"
            let arguments = sprintf "-f %s" projectFile

            let output = runConsoleApp arguments

            Assert.IsTrue(
                output.Contains(sprintf "Could not find the project file: %s on disk" projectFile), 
                sprintf "Output:\n%s" output)

        [<Test>]
        member __.FunctionsAsExpectedWithNonExistantFindReferencedProjectFile() = 
            let projectFile = getTestFilePath "referencesNonExistantProject.fsproj"
            let arguments = sprintf "-f %s" projectFile

            let output = runConsoleApp arguments
            
            Assert.IsTrue(
                output.Contains("Could not find file") || output.Contains("not found"), 
                sprintf "Output:\n%s" output)

        [<Test>]
        member __.FunctionalTestConsoleApplication() = 
            let projectFile = getTestFilePath "FSharpLint.FunctionalTest.TestedProject.fsproj"
            let arguments = sprintf "-f %s" projectFile

            let output = runConsoleApp arguments
            let errors = getErrorsFromOutput output

            for expectedError in expectedErrors do
                let containsExpectedError = List.exists (fun y -> y.Description = expectedError) errors
                Assert.True(
                    containsExpectedError, 
                    sprintf "Errors did not contain expected error:\n %s. Program output:\n %s" expectedError output)

            Assert.AreEqual(expectedErrors.Length, errors.Length)