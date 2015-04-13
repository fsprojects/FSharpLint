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

[<TestFixture>]
type TestConfiguration() =
    [<Test>]
    member self.``Ignore all files ignores any given file.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "*"
            ]

        IgnoreFiles.shouldFileBeIgnored @"D:\dog\source.fs" ignorePaths
            |> Assert.IsTrue

    [<Test>]
    member self.``Ignoring a file name not inside a path does not ignore the path``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "cat"
            ]

        IgnoreFiles.shouldFileBeIgnored @"D:\dog\source.fs" ignorePaths
            |> Assert.IsFalse

    [<Test>]
    member self.``Ignoring a file doesn't ignore a directory.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "dog"
            ]

        IgnoreFiles.shouldFileBeIgnored @"D:\dog\source.fs" ignorePaths
            |> Assert.IsFalse
            
    [<Test>]
    member self.``Ignoring a directory doesn't ignore a file.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "source.fs/"
            ]

        IgnoreFiles.shouldFileBeIgnored @"D:\dog\source.fs" ignorePaths
            |> Assert.IsFalse
    [<Test>]
    member self.``Ignoring all files in a given directory ignores a given file from the directory.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "dog/*"
            ]

        IgnoreFiles.shouldFileBeIgnored @"D:\dog\source.fs" ignorePaths
            |> Assert.IsTrue

    [<Test>]
    member self.``Ignoring a file that does not exist inside a directory that does exist does not ignore the file.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "dog/source1"
            ]

        IgnoreFiles.shouldFileBeIgnored @"D:\dog\source.fs" ignorePaths
            |> Assert.IsFalse

    [<Test>]
    member self.``Ignoring the contents of a directory and then negating a specific file ignores all files other than the negated file.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "dog/*"
                IgnoreFiles.parseIgnorePath "!source.*"
            ]

        IgnoreFiles.shouldFileBeIgnored @"D:\dog\source.fs" ignorePaths
            |> Assert.IsFalse

        IgnoreFiles.shouldFileBeIgnored @"D:\dog\source2.fs" ignorePaths
            |> Assert.IsTrue

    [<Test>]
    member self.``Ingoring a file that was previously negated ignores the file.``() = 
        let ignorePaths =
            [
                IgnoreFiles.parseIgnorePath "dog/*"
                IgnoreFiles.parseIgnorePath "!source.*"
                IgnoreFiles.parseIgnorePath "dog/*"
            ]

        IgnoreFiles.shouldFileBeIgnored @"D:\dog\source.fs" ignorePaths
            |> Assert.IsTrue

    [<Test>]
    member self.OverwriteMap() = 
        let mapToBeOverwrited = [ (1,"1"); (2,"2"); (3,"3"); (4,"5") ] |> Map.ofList

        let map = [ (2,"5"); (4,"1");  ] |> Map.ofList

        let expectedMap = [ (1,"1"); (2,"5"); (3,"3"); (4,"1")  ] |> Map.ofList

        Assert.AreEqual(expectedMap, overwriteMap mapToBeOverwrited map (fun _ x -> x))