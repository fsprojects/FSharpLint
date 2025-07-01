module FSharpLint.Console.Tests.TestApp

open System
open System.IO
open NUnit.Framework

let getErrorsFromOutput (output:string) =
    let splitOutput = output.Split([|Environment.NewLine|], StringSplitOptions.None)

    set [ for i in 1..splitOutput.Length - 1 do
            if splitOutput.[i].StartsWith "Error" then yield splitOutput.[i - 1] ]

type TemporaryFile(fileContent : string, extension) =
    let filename = Path.ChangeExtension(Path.GetTempFileName(), extension)
    do
        File.WriteAllText(filename, fileContent)

    member _.FileName = filename

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

        Assert.AreEqual(-1, returnCode)
        Assert.AreEqual(set ["Consider changing `Signature` to be prefixed with `I`."], errors)

    [<Test>]
    member _.``Lint source without any config, rule enabled in default config is triggered for given source.``() =
        let input = """
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """

        let (returnCode, errors) = main [| "lint"; input |]

        Assert.AreEqual(-1, returnCode)
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

        let (returnCode, errors) = main [| "lint"; "--lint-config"; config.FileName; input |]

        Assert.AreEqual(0, returnCode)
        Assert.AreEqual(Set.empty<string>, errors)

    [<Test>]
    member _.``Lint source with error suppressed, no error is given.``() =
        let input = """
        // fsharplint:disable-next-line
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """

        let (returnCode, errors) = main [| "lint"; input |]

        Assert.AreEqual(0, returnCode)
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

        let (returnCode, errors) = main [| "lint"; "--lint-config"; config.FileName; input |]

        Assert.AreEqual(-1, returnCode)
        Assert.AreEqual(set ["Use prefix syntax for generic type."], errors)

open FSharpLint.Console.Program

[<TestFixture>]
type TestFileTypeInference() =

    [<TestCase("test.fs", FileType.File, TestName = "inferFileType must recognize .fs files as File type")>]
    [<TestCase("script.fsx", FileType.File, TestName = "inferFileType must recognize .fsx files as File type")>]
    [<TestCase("MyProject.fsproj", FileType.Project, TestName = "inferFileType must recognize .fsproj files as Project type")>]
    [<TestCase("MySolution.sln", FileType.Solution, TestName = "inferFileType must recognize .sln files as Solution type")>]
    [<TestCase("MySolution.slnx", FileType.Solution, TestName = "inferFileType must recognize .slnx files as Solution type")>]
    [<TestCase("unknown.txt", FileType.Source, TestName = "inferFileType must treat unknown extensions as Source type")>]
    [<TestCase("noextension", FileType.Source, TestName = "inferFileType must treat files without extensions as Source type")>]
    [<TestCase("src/MyProject/Program.fs", FileType.File, TestName = "inferFileType must handle .fs files in directories correctly")>]
    [<TestCase(@"C:\Projects\MySolution.slnx", FileType.Solution, TestName = "inferFileType must handle .slnx files with full paths correctly")>]
    [<TestCase("../MyProject.fsproj", FileType.Project, TestName = "inferFileType must handle .fsproj files with relative paths correctly")>]
    member _.``File type inference test cases``(filename: string, expectedType: int) =
        let result = FSharpLint.Console.Program.inferFileType filename
        let expectedType = enum<FileType>(expectedType)
        Assert.AreEqual(expectedType, result)
