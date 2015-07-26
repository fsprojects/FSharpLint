(*
    FSharpLint, a linter for F#.
    Copyright (C) 2014 Matthew Mcveigh

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

module TestConfiguration

open NUnit.Framework
open FSharpLint.Framework.Configuration

type System.String with
    member path.ToPlatformIndependentPath() =
        path.Replace('\\', System.IO.Path.DirectorySeparatorChar)

    member this.RemoveWhitepsace() =
        System.Text.RegularExpressions.Regex.Replace(this, @"\s+", "")

[<TestFixture>]
type TestConfiguration() =
    [<Test>]
    member self.``Ignore all files ignores any given file.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "*"
            ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
            |> Assert.IsTrue

    [<Test>]
    member self.``Ignoring a file name not inside a path does not ignore the path``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "cat"
            ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
            |> Assert.IsFalse

    [<Test>]
    member self.``Ignoring a file doesn't ignore a directory.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "dog"
            ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths  path
            |> Assert.IsFalse
            
    [<Test>]
    member self.``Ignoring a directory doesn't ignore a file.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "source.fs/"
            ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
            |> Assert.IsFalse

    [<Test>]
    member self.``Ignoring all files in a given directory ignores a given file from the directory.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "dog/*"
            ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
            |> Assert.IsTrue

    [<Test>]
    member self.``Ignoring a file that does not exist inside a directory that does exist does not ignore the file.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "dog/source1"
            ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
            |> Assert.IsFalse

    [<Test>]
    member self.``Ignoring the contents of a directory and then negating a specific file ignores all files other than the negated file.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "dog/*"
                IgnoreFiles.parseIgnorePath "!source.*"
            ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
            |> Assert.IsFalse

        let path = @"D:\dog\source2.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
            |> Assert.IsTrue

    [<Test>]
    member self.``Ingoring a file that was previously negated ignores the file.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "dog/*"
                IgnoreFiles.parseIgnorePath "!source.*"
                IgnoreFiles.parseIgnorePath "dog/*"
            ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
            |> Assert.IsTrue

    [<Test>]
    member self.OverwriteMap() = 
        let mapToBeOverwrited = [ (1,"1"); (2,"2"); (3,"3"); (4,"5") ] |> Map.ofList

        let map = [ (2,"5"); (4,"1");  ] |> Map.ofList

        let expectedMap = [ (1,"1"); (2,"5"); (3,"3"); (4,"1")  ] |> Map.ofList

        Assert.AreEqual(expectedMap, overwriteMap mapToBeOverwrited map (fun _ x -> x))

    [<Test>]
    member self.``Empty config writes correct XML document``() = 
        let config =
            {
                UseTypeChecker = None
                IgnoreFiles = None
                Analysers = Map.empty
            }

        let doc = config.ToXmlDocument().ToString()

        let expectedXml = 
            """
<FSharpLintSettings xmlns="https://github.com/fsprojects/FSharpLint/blob/master/ConfigurationSchema.xsd">
    <Analysers />
</FSharpLintSettings>"""

        Assert.AreEqual(expectedXml.RemoveWhitepsace(), doc.RemoveWhitepsace())

    [<Test>]
    member self.``Config specifying to use type checker writes correct XML document``() = 
        let config =
            {
                UseTypeChecker = Some(true)
                IgnoreFiles = None
                Analysers = Map.empty
            }

        let doc = config.ToXmlDocument().ToString()

        let expectedXml = 
            """
<FSharpLintSettings xmlns="https://github.com/fsprojects/FSharpLint/blob/master/ConfigurationSchema.xsd">
    <UseTypeChecker>True</UseTypeChecker>
    <Analysers />
</FSharpLintSettings>"""

        Assert.AreEqual(expectedXml.RemoveWhitepsace(), doc.RemoveWhitepsace())

    [<Test>]
    member self.``Config specifying files to ignore writes correct XML document``() = 
        let config =
            {
                UseTypeChecker = None
                IgnoreFiles = Some({
                                    Update = IgnoreFiles.IgnoreFilesUpdate.Add
                                    Files = []
                                    Content = "assemblyinfo.*"})
                Analysers = Map.empty
            }

        let doc = config.ToXmlDocument().ToString()

        let expectedXml = 
            """
<FSharpLintSettings xmlns="https://github.com/fsprojects/FSharpLint/blob/master/ConfigurationSchema.xsd">
    <IgnoreFiles Update="Add">
        <![CDATA[
          assemblyinfo.*
        ]]>
    </IgnoreFiles>
    <Analysers />
</FSharpLintSettings>"""

        Assert.AreEqual(expectedXml.RemoveWhitepsace(), doc.RemoveWhitepsace())