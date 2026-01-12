module FSharpLint.Console.Tests.TestApp

open System
open System.IO
open NUnit.Framework
open FSharpLint.Console.Program

let getErrorsFromOutput (output:string) =
    let splitOutput = output.Split([|Environment.NewLine|], StringSplitOptions.None)

    set [ for index in 1..splitOutput.Length - 1 do
            if splitOutput.[index].StartsWith "Error" then yield splitOutput.[index - 1] ]

type TemporaryFile(fileContent : string, extension) =
    let filename = Path.ChangeExtension(Path.GetTempFileName(), extension)
    do
        File.WriteAllText(filename, fileContent)

    member val FileName = filename

    interface System.IDisposable with
        member _.Dispose() =
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
    member _.``Lint file, expected rules are triggered.``() =
        let fileContent = """
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """
        use input = new TemporaryFile(fileContent, "fs")

        let (returnCode, errors) = main [| "lint"; input.FileName |]

        Assert.AreEqual(int ExitCode.Failure, returnCode)
        Assert.AreEqual(set ["Consider changing `Signature` to be prefixed with `I`."], errors)

    [<Test>]
    member _.``Lint source without any config, rule enabled in default config is triggered for given source.``() =
        let input = """
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """

        let (returnCode, errors) = main [| "lint"; "--file-type"; "source"; input |]

        Assert.AreEqual(int ExitCode.Failure, returnCode)
        Assert.AreEqual(set ["Consider changing `Signature` to be prefixed with `I`."], errors)

    [<Test>]
    member _.``Lint source with valid config to disable rule, disabled rule is not triggered for given source.``() =
        let fileContent = """
        {
            "InterfaceNames": {
                "enabled": false
            }
        }
        """
        use config = new TemporaryFile(fileContent, "json")

        let input = """
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """

        let (returnCode, errors) = main [| "lint"; "--lint-config"; config.FileName; "--file-type"; "source"; input |]

        Assert.AreEqual(int ExitCode.Success, returnCode)
        Assert.AreEqual(Set.empty<string>, errors)

    [<Test>]
    member _.``Lint source with error suppressed, no error is given.``() =
        let input = """
        // fsharplint:disable-next-line
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """

        let (returnCode, errors) = main [| "lint"; "--file-type"; "source"; input |]

        Assert.AreEqual(int ExitCode.Success, returnCode)
        Assert.AreEqual(Set.empty<string>, errors)

    [<Test>]
    member _.``Regression test: typePrefixing rule with old config format should still work``() =
        let fileContent = """
        {
            "typePrefixing": {
                "enabled": true
            }
        }
        """
        use config = new TemporaryFile(fileContent, "json")

        let input = """
        module Program

        type X = int Generic
        """

        let (returnCode, errors) = main [| "lint"; "--lint-config"; config.FileName; "--file-type"; "source"; input |]

        Assert.AreEqual(int ExitCode.Failure, returnCode)
        Assert.AreEqual(set ["Use prefix syntax for generic type."], errors)

[<TestFixture>]
type TestFileTypeInference() =
    
    [<TestCase("test.fs", FileType.File, TestName = "inferFileType must recognize .fs files as File type")>]
    [<TestCase("script.fsx", FileType.File, TestName = "inferFileType must recognize .fsx files as File type")>]
    [<TestCase("MyProject.fsproj", FileType.Project, TestName = "inferFileType must recognize .fsproj files as Project type")>]
    [<TestCase("MySolution.sln", FileType.Solution, TestName = "inferFileType must recognize .sln files as Solution type")>]
    [<TestCase("MySolution.slnx", FileType.Solution, TestName = "inferFileType must recognize .slnx files as Solution type")>]
    [<TestCase("MySolution.slnf", FileType.Solution, TestName = "inferFileType must recognize .slnf files as Solution type")>]
    [<TestCase("src/MyProject/Program.fs", FileType.File, TestName = "inferFileType must handle .fs files in directories correctly")>]
    [<TestCase(@"C:\Projects\MySolution.slnx", FileType.Solution, TestName = "inferFileType must handle .slnx files with full paths correctly")>]
    [<TestCase(@"C:\Projects\MySolution.slnf", FileType.Solution, TestName = "inferFileType must handle .slnf files with full paths correctly")>]
    [<TestCase("../MyProject.fsproj", FileType.Project, TestName = "inferFileType must handle .fsproj files with relative paths correctly")>]
    [<TestCase("*.fs", FileType.Wildcard, TestName = "inferFileType must recognize wildcard patterns with * as Wildcard type")>]
    [<TestCase("**/*.fs", FileType.Wildcard, TestName = "inferFileType must recognize recursive wildcard patterns as Wildcard type")>]
    [<TestCase("src/**/*.fsx", FileType.Wildcard, TestName = "inferFileType must recognize subdirectory recursive wildcard patterns as Wildcard type")>]
    [<TestCase("test?.fs", FileType.Wildcard, TestName = "inferFileType must recognize wildcard patterns with ? as Wildcard type")>]
    member _.``File type inference test cases``(filename: string, expectedType: int) =
        let result = FSharpLint.Console.Program.inferFileType filename
        let expectedType = Some <| enum<FileType>(expectedType)
        Assert.AreEqual(expectedType, result)

    [<TestCase("unknown.txt", TestName = "inferFileType must treat unknown extensions as undecided")>]
    [<TestCase("noextension", TestName = "inferFileType must treat files without extensions as undecided")>]
    member _.``File type inference undecided test cases``(filename: string) =
        let result = FSharpLint.Console.Program.inferFileType filename
        let expectedType = None
        Assert.AreEqual(expectedType, result)

[<TestFixture>]
type TestWildcardExpansion() =
    
    [<Test>]
    member _.``expandWildcard finds .fs files in current directory``() =
        use file1 = new TemporaryFile("module Test1", "fs")
        use file2 = new TemporaryFile("module Test2", "fs")
        let dir = Path.GetDirectoryName(file1.FileName)
        let pattern = Path.Combine(dir, "*.fs")
        
        let results = expandWildcard pattern
        
        Assert.That(results, Is.Not.Empty)
        Assert.That(results, Does.Contain(file1.FileName))
        Assert.That(results, Does.Contain(file2.FileName))
    
    [<Test>]
    member _.``expandWildcard finds .fsx files``() =
        use file1 = new TemporaryFile("printfn \"test\"", "fsx")
        let dir = Path.GetDirectoryName(file1.FileName)
        let pattern = Path.Combine(dir, "*.fsx")
        
        let results = expandWildcard pattern
        
        Assert.That(results, Does.Contain(file1.FileName))
    
    [<Test>]
    member _.``expandWildcard returns empty list for non-existent directory``() =
        let pattern = "nonexistent_directory/*.fs"
        
        let results = expandWildcard pattern
        
        Assert.That(results, Is.Empty)
