namespace FSharpLint.FunctionalTest

// fsharplint:disable TupleIndentation

module Tests =

    open FSharpLint.Framework
    open System
    open System.Diagnostics
    open System.IO
    open NUnit.Framework

    let (</>) x y = Path.Combine(x, y)

    let basePath = TestContext.CurrentContext.TestDirectory </> ".." </> ".." </> ".." </> ".." </> ".."

    type Error =
        { Description:string
          Location:string
          Code:string }

        override this.ToString() =
            $"{{%s{Environment.NewLine}    Description=\"%s{this.Description}\"%s{Environment.NewLine}    Location=\"%s{this.Location}\"%s{Environment.NewLine}    Code=\"%s{this.Code}\"%s{Environment.NewLine}}}"

    let dotnetFslint arguments =
        let configDirName =
            #if DEBUG
                "Debug"
            #else
                "Release"
            #endif

        let binDir =
            basePath </> "src" </> "FSharpLint.Console" </> "bin" </> configDirName |> DirectoryInfo

        let dll =
            let dllDir =
                binDir.EnumerateDirectories()
                |> Seq.sortByDescending _.Name
                |> Seq.tryHead
            match dllDir with
            | Some dir -> dir.FullName </> "fsharplint.dll"
            | None -> failwithf "No target framework folder found in %s" binDir.FullName

        let startInfo = ProcessStartInfo
                                (FileName = "dotnet",
                                 Arguments = dll + " " + arguments,
                                 RedirectStandardOutput = true,
                                 RedirectStandardError = true,
                                 UseShellExecute = false,
                                 WorkingDirectory = (basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore"))

        use app = Process.Start(startInfo)
        let output = app.StandardOutput.ReadToEnd()
        let output = output + app.StandardError.ReadToEnd()
        app.WaitForExit()
        output

    let getErrorsFromOutput (output:string) =
        let splitOutput = output.Split([|Environment.NewLine|], StringSplitOptions.None)

        set [ for line in splitOutput do
                if line.StartsWith "`" && line.Contains "might be able to be refactored into" then
                    yield line ]

    let expectedErrors =
        set [
          "`not (a = b)` might be able to be refactored into `a <> b`."
          "`not (a <> b)` might be able to be refactored into `a = b`."
          "`fun x -> x` might be able to be refactored into `id`."
          "`not true` might be able to be refactored into `false`."
          "`not false` might be able to be refactored into `true`."
          "`List.fold ( + ) 0 x` might be able to be refactored into `List.sum x`."
          "`a <> true` might be able to be refactored into `not a`."
          "`x = null` might be able to be refactored into `isNull x`."
          "`List.head (List.sort x)` might be able to be refactored into `List.min x`." ]

    [<TestFixture(Category = "Acceptance Tests")>]
    type TestConsoleApplication() =
        let projectPath =
            basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore"

        [<Test>]
        member _.InvalidConfig() =
            let projectFile = projectPath </> "FSharpLint.FunctionalTest.TestedProject.NetCore.fsproj"
            let lintConfigPath = projectPath </> "fsharplint.json"
            let arguments = $"lint --lint-config %s{lintConfigPath} %s{projectFile}"

            File.WriteAllText(lintConfigPath, "invalid config file contents")

            let output = dotnetFslint arguments

            File.Delete(projectPath </> "fsharplint.json")

            Assert.IsTrue(output.Contains("Failed while reading from config at run time"), $"Output:%s{Environment.NewLine}%s{output}")

        [<Test>]
        member _.UnableToFindProjectFile() =
            let projectFile = projectPath </> "iuniubi.fsproj"
            let arguments = $"lint %s{projectFile}"

            let output = dotnetFslint arguments

            Assert.IsTrue(
                output.Contains($"Could not find the file: %s{projectFile} on disk"),
                $"Output:%s{Environment.NewLine}%s{output}")

        [<Test>]
        member _.FunctionalTestConsoleApplication() =
            let projectFile = projectPath </> "FSharpLint.FunctionalTest.TestedProject.NetCore.fsproj"
            let arguments = $"lint {projectFile}"

            let output = dotnetFslint arguments
            let errors = getErrorsFromOutput output

            let expectedMissing = Set.difference expectedErrors errors
            let notExpected = Set.difference errors expectedErrors
            let expectedMissingStr = String.concat "," expectedMissing
            let notExpectedStr = String.concat "," notExpected

            Assert.AreEqual(expectedErrors, errors,
                $"Did not find the following expected errors: [{expectedMissingStr}]\n" +
                $"Found the following unexpected warnings: [{notExpectedStr}]\n" +
                $"Complete output: {output}")

        [<Test>]
        member _.FunctionalTestConsoleApplicationSolution() =
            let solutionFile = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.sln"
            let arguments = $"lint {solutionFile}"

            let output = dotnetFslint arguments
            let errors = getErrorsFromOutput output

            let expectedMissing = Set.difference expectedErrors errors
            let notExpected = Set.difference errors expectedErrors
            let expectedMissingStr = String.concat "," expectedMissing
            let notExpectedStr = String.concat "," notExpected

            Assert.AreEqual(expectedErrors, errors,
                $"Did not find the following expected errors: [{expectedMissingStr}]\n" +
                $"Found the following unexpected warnings: [{notExpectedStr}]\n" +
                $"Complete output: {output}")
