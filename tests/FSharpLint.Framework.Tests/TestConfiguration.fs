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
open FSharpLint.Framework.HintParser
open FSharpLint.Framework.Configuration
open Management

type System.String with
    member path.ToPlatformIndependentPath() =
        path.Replace('\\', System.IO.Path.DirectorySeparatorChar)

    member this.RemoveWhitepsace() =
        System.Text.RegularExpressions.Regex.Replace(this, @"\s+", "")

let emptyLoadedConfigs = { LoadedConfigs = Map.ofList []; PathsAdded = [] }

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
    member __.OverwriteMap() = 
        let mapToBeOverwrited = [ (1,"1"); (2,"2"); (3,"3"); (4,"5") ] |> Map.ofList

        let map = [ (2,"5"); (4,"1");  ] |> Map.ofList

        let expectedMap = [ (1,"1"); (2,"5"); (3,"3"); (4,"1")  ] |> Map.ofList

        Assert.AreEqual(expectedMap, overwriteMap mapToBeOverwrited map (fun _ x -> x))

    [<Test>]
    member __.``Empty config writes correct XML document``() = 
        let config =
            { UseTypeChecker = None
              IgnoreFiles = None
              Analysers = Map.empty }

        let doc = config.ToXmlDocument().ToString()

        let expectedXml = 
            """
<FSharpLintSettings xmlns="https://github.com/fsprojects/FSharpLint/blob/master/ConfigurationSchema.xsd">
    <Analysers />
</FSharpLintSettings>"""

        Assert.AreEqual(expectedXml.RemoveWhitepsace(), doc.RemoveWhitepsace())

    [<Test>]
    member __.``Config specifying to use type checker writes correct XML document``() = 
        let config =
            { UseTypeChecker = Some(true)
              IgnoreFiles = None
              Analysers = Map.empty }

        let doc = config.ToXmlDocument().ToString()

        let expectedXml = 
            """
<FSharpLintSettings xmlns="https://github.com/fsprojects/FSharpLint/blob/master/ConfigurationSchema.xsd">
    <UseTypeChecker>True</UseTypeChecker>
    <Analysers />
</FSharpLintSettings>"""

        Assert.AreEqual(expectedXml.RemoveWhitepsace(), doc.RemoveWhitepsace())

    [<Test>]
    member __.``Config specifying files to ignore writes correct XML document``() = 
        let config =
            { UseTypeChecker = None
              IgnoreFiles = Some({ Update = IgnoreFiles.IgnoreFilesUpdate.Add
                                   Files = []
                                   Content = "assemblyinfo.*"})
              Analysers = Map.empty }

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

    [<Test>]
    member __.``Config specifying an analyser writes correct XML document``() = 
        let rule = { Rule.Settings = [ ("Enabled", Enabled(true))  ] |> Map.ofList }

        let analyser =
            { Settings = [ ("Enabled", Enabled(true))  ] |> Map.ofList
              Rules = [ ("ReimplementsFunction", rule)  ] |> Map.ofList }

        let config =
            { UseTypeChecker = None
              IgnoreFiles = None
              Analysers = [ ("FunctionReimplementation", analyser)  ] |> Map.ofList }

        let doc = config.ToXmlDocument().ToString()

        let expectedXml = 
            """
<FSharpLintSettings xmlns="https://github.com/fsprojects/FSharpLint/blob/master/ConfigurationSchema.xsd">
    <Analysers>
        <FunctionReimplementation>
          <Rules>
            <ReimplementsFunction>
              <Enabled>True</Enabled>
            </ReimplementsFunction>
          </Rules>
          <Enabled>True</Enabled>
        </FunctionReimplementation>
    </Analysers>
</FSharpLintSettings>"""

        Assert.AreEqual(expectedXml.RemoveWhitepsace(), doc.RemoveWhitepsace())

    [<Test>]
    member __.``Config specifying hints writes correct XML document``() = 
        let parsedHint = { Match = Expression.Wildcard; Suggestion = Suggestion.Expr(Expression.Wildcard) }

        let hints =
            [ { Hint = "not (a =  b) ===> a <> b"; ParsedHint = parsedHint }
              { Hint = "not (a <> b) ===> a = b"; ParsedHint = parsedHint } ]

        let analyser =
            { Settings = [ ("Hints", Hints(hints))  ] |> Map.ofList
              Rules = Map.empty }

        let config =
            { UseTypeChecker = None
              IgnoreFiles = None
              Analysers = [ ("Hints", analyser)  ] |> Map.ofList }

        let doc = config.ToXmlDocument().ToString()

        let expectedXml = 
            """
<FSharpLintSettings xmlns="https://github.com/fsprojects/FSharpLint/blob/master/ConfigurationSchema.xsd">
    <Analysers>
        <Hints>
            <Rules />
            <Hints>
                <![CDATA[
                    not (a =  b) ===> a <> b
                    not (a <> b) ===> a = b
                ]]>
            </Hints>
        </Hints>
    </Analysers>
</FSharpLintSettings>"""

        Assert.AreEqual(expectedXml.RemoveWhitepsace(), doc.RemoveWhitepsace())

    [<Test>]
    member __.``Load two paths with same root with a common directory; loads expected tree.``() = 
        let expectedLoadedConfigs = 
            { LoadedConfigs = 
                [ (["C:"], None)
                  (["C:"; "Dog"], None)
                  (["C:"; "Dog"; "Goat"], None)
                  (["C:"; "Dog"; "Cat"], None) ] |> Map.ofList
              PathsAdded = [ ["C:"; "Dog"; "Cat"]; ["C:"; "Dog"; "Goat"] ] }

        let loadedConfigs = Management.addPath (fun _ -> None) emptyLoadedConfigs [ "C:"; "Dog"; "Goat" ]

        let loadedConfigs = Management.addPath (fun _ -> None) loadedConfigs [ "C:"; "Dog"; "Cat" ]

        Assert.AreEqual(expectedLoadedConfigs, loadedConfigs)

    [<Test>]
    member __.``Load two paths with different roots; loads expected tree.``() = 
        let expectedLoadedConfigs = 
            { LoadedConfigs = 
                [ (["D:"], None)
                  (["D:"; "Dog"], None)
                  (["C:"], None)
                  (["C:"; "Dog"], None) ] |> Map.ofList
              PathsAdded = [ ["D:"; "Dog"]; ["C:"; "Dog"] ] }

        let loadedConfigs = Management.addPath (fun _ -> None) emptyLoadedConfigs [ "C:"; "Dog" ]

        let loadedConfigs = Management.addPath (fun _ -> None) loadedConfigs [ "D:"; "Dog" ]

        Assert.AreEqual(expectedLoadedConfigs, loadedConfigs)

    [<Test>]
    member __.``Load two paths with one a directory deeper than the other; loads expected tree.``() = 
        let expectedLoadedConfigs = 
            { LoadedConfigs = 
                [ (["C:"], None)
                  (["C:"; "Dog"], None)
                  (["C:"; "Dog"; "Goat"], None)
                  (["C:"; "Dog"; "Goat"; "Cat"], None) ] |> Map.ofList
              PathsAdded = [ ["C:"; "Dog"; "Goat"; "Cat"]; ["C:"; "Dog"; "Goat"] ] }

        let loadedConfigs = Management.addPath (fun _ -> None) emptyLoadedConfigs ["C:"; "Dog"; "Goat"]

        let loadedConfigs = Management.addPath (fun _ -> None) loadedConfigs ["C:"; "Dog"; "Goat"; "Cat" ]

        Assert.AreEqual(expectedLoadedConfigs, loadedConfigs)

    [<Test>]
    member __.``Removing paths remove expected loaded configs.``() = 
        let loadedConfigs = 
            { LoadedConfigs = 
                [ (["C:"], None)
                  (["C:"; "Dog"], None)
                  (["C:"; "Dog"; "Goat"], None)
                  (["C:"; "Dog"; "Goat"; "Cat"], None) ] |> Map.ofList
              PathsAdded = [ ["C:"; "Dog"; "Goat"; "Cat"]; ["C:"; "Dog"; "Goat"] ] }

        let updatedLoadedConfigs = 
            Management.removePath loadedConfigs [ "C:"; "Dog"; "Goat"; "Cat" ]

        let expectedLoadedConfigs = 
            { LoadedConfigs = 
                [ (["C:"], None)
                  (["C:"; "Dog"], None)
                  (["C:"; "Dog"; "Goat"], None) ] |> Map.ofList
              PathsAdded = [ ["C:"; "Dog"; "Goat"] ] }

        Assert.AreEqual(expectedLoadedConfigs, updatedLoadedConfigs)

        let updatedLoadedConfigs = 
            Management.removePath updatedLoadedConfigs [ "C:"; "Dog"; "Goat" ]

        let expectedLoadedConfigs = 
            { LoadedConfigs = [] |> Map.ofList
              PathsAdded = [] }

        Assert.AreEqual(expectedLoadedConfigs, updatedLoadedConfigs)

    [<Test>]
    member __.``Deepest common path is found when preferred path not found``() = 
        let loadedConfigs = 
            { LoadedConfigs = [] |> Map.ofList
              PathsAdded = 
                [ ["C:"; "Dog"; "Goat"; "Cat"]
                  ["C:"; "Dog"; "Goat"]
                  ["C:"; "Dog"; "Goat"; "Shrimp"] ] }

        let path = commonPath loadedConfigs ["C:";"NonExistant"]

        Assert.AreEqual(Some(["C:"; "Dog"; "Goat"]), path)

    [<Test>]
    member __.``Preferred path is returned when it is a common path``() = 
        let loadedConfigs = 
            { LoadedConfigs = [] |> Map.ofList
              PathsAdded = 
                [ ["C:"; "Dog"; "Goat"; "Cat"]
                  ["C:"; "Dog"; "Goat"]
                  ["C:"; "Dog"; "Goat"; "Shrimp"] ] }

        let path = commonPath loadedConfigs ["C:";"Dog"]

        Assert.AreEqual(Some(["C:"; "Dog"]), path)

    [<Test>]
    member __.``No path is returned when there is no common path``() = 
        let loadedConfigs = 
            { LoadedConfigs = [] |> Map.ofList
              PathsAdded = 
                [ ["C:"; "Dog"; "Goat"; "Cat"]
                  ["D:"; "Dog"; "Goat"; "Shrimp"] ] }

        let path = commonPath loadedConfigs ["C:";"Dog"]

        Assert.AreEqual(None, path)

    [<Test>]
    member __.``No path is returned when there are no paths added``() = 
        let loadedConfigs = 
            { LoadedConfigs = [] |> Map.ofList
              PathsAdded = [] }

        let path = commonPath loadedConfigs ["C:";"Dog"]

        Assert.AreEqual(None, path)

    [<Test>]
    member __.``Default configuration returned when there are no paths added``() = 
        let loadedConfigs = 
            { LoadedConfigs = [] |> Map.ofList
              PathsAdded = [] }

        let config = getConfig loadedConfigs ["C:";"Dog"]

        Assert.AreEqual(Some defaultConfiguration, config)

    [<Test>]
    member __.``Overridden configuration returned when there is a path added``() = 
        let loadedConfig = 
            { UseTypeChecker = None
              IgnoreFiles = None
              Analysers = 
                ["Typography", { Settings = [("Enabled", Enabled(false))] |> Map.ofList
                                 Rules = [] |> Map.ofList }] |> Map.ofList }

        let loadedConfigs = 
            { LoadedConfigs = [(["C:"], Some(loadedConfig))] |> Map.ofList
              PathsAdded = [["C:"]] }

        let config = getConfig loadedConfigs ["C:";"Dog"]

        let expectedConfig = overrideConfiguration defaultConfiguration loadedConfig

        Assert.AreEqual(Some expectedConfig, config)

    [<Test>]
    member __.``Update config updates differences``() = 
        let configFromAnalysers analysers =
            { UseTypeChecker = None
              IgnoreFiles = None
              Analysers = analysers }
            
        let partialConfigToUpdate = 
            [ "Dog", { Settings = [("Enabled", Enabled(false))] |> Map.ofList
                       Rules = [] |> Map.ofList } ] 
            |> Map.ofList 
            |> configFromAnalysers
            
        let fullUpdatedConfig = 
            [ "Typography", { Settings = [("Enabled", Enabled(false))] |> Map.ofList
                              Rules = [] |> Map.ofList } 
              "Dog", { Settings = [("Enabled", Enabled(true))] |> Map.ofList
                       Rules = [("Woofs", { Rule.Settings = [("Enabled", Enabled(true))] |> Map.ofList })] |> Map.ofList } ] 
            |> Map.ofList 
            |> configFromAnalysers
            
        let fullConfigToUpdate = 
            [ "Typography", { Settings = [("Enabled", Enabled(false))] |> Map.ofList
                              Rules = [] |> Map.ofList } 
              "Dog", { Settings = [("Enabled", Enabled(false))] |> Map.ofList
                       Rules = [] |> Map.ofList } ] 
            |> Map.ofList 
            |> configFromAnalysers

        let updatedConfig = updateConfigMap fullUpdatedConfig fullConfigToUpdate partialConfigToUpdate
            
        let expectedConfig = 
             [ "Dog", { Settings = [("Enabled", Enabled(true))] |> Map.ofList
                        Rules = [("Woofs", { Rule.Settings = [("Enabled", Enabled(true))] |> Map.ofList })] |> Map.ofList } ] 
            |> Map.ofList 
            |> configFromAnalysers

        Assert.AreEqual(expectedConfig, updatedConfig)
