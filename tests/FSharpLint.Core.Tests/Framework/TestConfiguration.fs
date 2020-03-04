module TestConfiguration

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Framework.Configuration

type System.String with
    member path.ToPlatformIndependentPath() =
        path.Replace('\\', System.IO.Path.DirectorySeparatorChar)

    member this.RemoveWhitepsace() =
        System.Text.RegularExpressions.Regex.Replace(this, @"\s+", "")

let configWithHints hints =
     { Configuration.Zero with hints = hints }


[<TestFixture>]
type TestConfiguration() =
    [<Test>]
    member __.``Ignore all files ignores any given file.``() =
        let ignorePaths = [ IgnoreFiles.parseIgnorePath "*" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsTrue

    [<Test>]
    member __.``Ignoring a file name not inside a path does not ignore the path``() =
        let ignorePaths = [ IgnoreFiles.parseIgnorePath "cat" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsFalse

    [<Test>]
    member __.``Ignoring a file doesn't ignore a directory.``() =
        let ignorePaths = [ IgnoreFiles.parseIgnorePath "dog" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths  path
        |> Assert.IsFalse

    [<Test>]
    member __.``Ignoring a directory doesn't ignore a file.``() =
        let ignorePaths = [ IgnoreFiles.parseIgnorePath "source.fs/" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsFalse

    [<Test>]
    member __.``Ignoring all files in a given directory ignores a given file from the directory.``() =
        let ignorePaths = [ IgnoreFiles.parseIgnorePath "dog/*" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsTrue

    [<Test>]
    member __.``Ignoring a file that does not exist inside a directory that does exist does not ignore the file.``() =
        let ignorePaths = [ IgnoreFiles.parseIgnorePath "dog/source1" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsFalse

    [<Test>]
    member __.``Ignoring the contents of a directory and then negating a specific file ignores all files other than the negated file.``() =
        let ignorePaths =
            [ IgnoreFiles.parseIgnorePath "dog/*"
              IgnoreFiles.parseIgnorePath "!source.*" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsFalse

        let path = @"D:\dog\source2.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsTrue

    [<Test>]
    member __.``Ingoring a file that was previously negated ignores the file.``() =
        let ignorePaths =
            [ IgnoreFiles.parseIgnorePath "dog/*"
              IgnoreFiles.parseIgnorePath "!source.*"
              IgnoreFiles.parseIgnorePath "dog/*" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsTrue

    [<Test>]
    member __.``Empty config writes correct JSON document`` () =
        let resultJson = serializeConfig Configuration.Zero

        let expectedJson = "{}"

        Assert.AreEqual(expectedJson.RemoveWhitepsace(), resultJson.RemoveWhitepsace())

    [<Test>]
    member __.``Config specifying files to ignore writes correct JSON document`` () =
        let config = { Configuration.Zero with ignoreFiles = Some [| "assemblyinfo.*" |] }

        let resultJson = serializeConfig config

        let expectedJson =
            """{
    "ignoreFiles": ["assemblyinfo.*"] }
"""

        Assert.AreEqual(expectedJson.RemoveWhitepsace(), resultJson.RemoveWhitepsace())

    [<Test>]
    member __.``Config specifying rule writes correct JSON document`` () =
        let config = {
            Configuration.Zero with
                typography =
                    Some { TypographyConfig.maxCharactersOnLine = Some { RuleConfig.enabled = true; config = Some { MaxCharactersOnLine.Config.maxCharactersOnLine = 4 } }
                           indentation = None
                           trailingWhitespaceOnLine = None
                           maxLinesInFile = None
                           trailingNewLineInFile = None
                           noTabCharacters = None
                    }
            }

        let resultJson = serializeConfig config

        let expectedJson =
            """{
    "typography": {
        "maxCharactersOnLine": {
            "enabled": true,
            "config": {
                "maxCharactersOnLine": 4
            }
        }
    }
}
"""

        Assert.AreEqual(expectedJson.RemoveWhitepsace(), resultJson.RemoveWhitepsace())

    [<Test>]
    member __.``Config specifying hints writes correct JSON document`` () =
        let config = {
            Configuration.Zero with
                hints =
                    { HintConfig.add = Some [| "not (a =  b) ===> a <> b"; "not (a <> b) ===> a = b" |]
                      ignore = Some [| "x = true ===> x" |] } |> Some
            }

        let resultJson = serializeConfig config

        let expectedJson =
            """{
    "hints": {
        "add": [
            "not (a =  b) ===> a <> b",
            "not (a <> b) ===> a = b"
        ],
        "ignore": ["x = true ===> x"]
    }
}
"""

        Assert.AreEqual(expectedJson.RemoveWhitepsace(), resultJson.RemoveWhitepsace())