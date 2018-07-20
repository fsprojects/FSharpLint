namespace FSharpLint.FunctionalTest

module TestFakeTask =

    open System.Diagnostics
    open System.IO
    open System.Text
    open NUnit.Framework
    open TestPackageHelper

    let runFake() =
        let fakeExe = basePath </> "packages" </> "tools" </> "FAKE" </> "tools" </> "FAKE.exe"

        let workingDirectory = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject"

        let buildFile =
            if Fake.EnvironmentHelper.isMono then
                FileInfo(Path.Combine(workingDirectory, "testLintViaFake.fsx")).FullName
            else
                "testLintViaFake.fsx"

        let file = if Fake.EnvironmentHelper.isMono then "mono" else fakeExe

        let arguments =
            if Fake.EnvironmentHelper.isMono then
                sprintf "%s %s" (FileInfo(fakeExe).FullName) buildFile
            else
                buildFile 

        let startInfo = ProcessStartInfo
                                (FileName = file,
                                 Arguments = arguments,
                                 RedirectStandardOutput = true,
                                 WorkingDirectory = workingDirectory,
                                 UseShellExecute = false)

        use app = Process.Start(startInfo)

        let output = StringBuilder()
        
        while not app.StandardOutput.EndOfStream do
            app.StandardOutput.ReadLine() |> output.Append |> ignore

        output.ToString()
    
    [<TestFixture(Category = "Acceptance Tests")>]
    type TestFakeTask() =
        [<SetUp>]
        member __.CopyFSharpLintTaskFiles() = TestPackageHelper.copyFSharpLintTaskFiles "FSharpLintFakeTaskTest"

        [<Test>]
        member __.FunctionalTestFakeTask() = 
            let output = runFake()

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

            let missingErrors = expectedErrors |> List.filter (output.Contains >> not)

            let failInfo = 
                sprintf 
                    "FAKE output didn't contain expected lint warnings.\nExpected %s.\noutput: %s" 
                    (String.concat "; " missingErrors)
                    output
                
            Assert.IsTrue(missingErrors.IsEmpty, failInfo)