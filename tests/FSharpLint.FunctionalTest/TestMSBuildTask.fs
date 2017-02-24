namespace FSharpLint.FunctionalTest

module TestMSBuildTask =

    open System.Diagnostics
    open System.Text
    open NUnit.Framework
    open TestPackageHelper

    let msbuildProject projectFile =
        let startInfo = ProcessStartInfo
                                (FileName = Fake.MSBuildHelper.msBuildExe,
                                 Arguments = projectFile,
                                 RedirectStandardOutput = true,
                                 UseShellExecute = false)

        use app = Process.Start(startInfo)

        let output = StringBuilder()
        
        while not app.StandardOutput.EndOfStream do
            app.StandardOutput.ReadLine() |> output.Append |> ignore

        output.ToString()
    
    [<TestFixture(Category = "Acceptance Tests")>]
    type TestMSBuildTask() =
        [<SetUp>]
        member __.CopyFSharpLintTaskFiles() = TestPackageHelper.copyFSharpLintTaskFiles "FSharpLintMSBuildTaskTest"

        [<Test>]
        member __.FunctionalTestMSBuildTask() = 
            let projectFile = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> 
                              "FSharpLint.FunctionalTest.TestedProjectMSBuildTask.fsproj"

            let output = msbuildProject projectFile

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
                  
            let allFound = expectedErrors |> List.forall output.Contains

            let failInfo = sprintf "MSBuild output didn't contain expected lint warnings. output: %s" output
                
            Assert.IsTrue(allFound, failInfo)