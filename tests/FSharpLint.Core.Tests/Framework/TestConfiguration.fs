module TestConfiguration

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Application.ConfigurationManager
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.Configuration.Management

type System.String with
    member path.ToPlatformIndependentPath() =
        path.Replace('\\', System.IO.Path.DirectorySeparatorChar)

    member this.RemoveWhitepsace() =
        System.Text.RegularExpressions.Regex.Replace(this, @"\s+", "")
        
let emptyLoadedConfigs = { LoadedConfigs = Map.empty; PathsAdded = []; GlobalConfigs = [] }
 
let configWithHints hints =
     {
        Configuration.formatting = None
        conventions = None
        typography = None
        ignoreFiles = Array.empty
        hints = hints
    }
 
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
        let config = {
            Configuration.formatting = None
            conventions = None
            typography = None
            ignoreFiles = Array.empty
            hints = Array.empty
        }
        
        let resultJson = serializeConfig config
        
        let expectedJson =
            """{
    "ignoreFiles": [],
    "hints": []
}
"""

        Assert.AreEqual(expectedJson.RemoveWhitepsace(), resultJson.RemoveWhitepsace())
        
    [<Test>]
    member __.``Config specifying files to ignore writes correct JSON document`` () =
        let config = {
            Configuration.formatting = None
            conventions = None
            typography = None
            ignoreFiles = [| "assemblyinfo.*" |]
            hints = Array.empty
        }
        
        let resultJson = serializeConfig config
        
        let expectedJson =
            """{
    "ignoreFiles": ["assemblyinfo.*"],
    "hints": []
}
"""

        Assert.AreEqual(expectedJson.RemoveWhitepsace(), resultJson.RemoveWhitepsace())
        
    [<Test>]
    member __.``Config specifying rule writes correct JSON document`` () =
        let config = {
            Configuration.formatting = None
            conventions = None
            typography =
                Some { TypographyConfig.indentation = Some { RuleConfig.enabled = true; config = Some { Indentation.Config.numberOfIndentationSpaces = 4 } }
                       maxCharactersOnLine = None
                       trailingWhitespaceOnLine = None
                       maxLinesInFile = None
                       trailingNewLineInFile = None
                       noTabCharacters = None
                }
            ignoreFiles = Array.empty
            hints = Array.empty
        }
        
        let resultJson = serializeConfig config
        
        let expectedJson =
            """{
    "ignoreFiles": [],
    "typography": {
        "indentation": {
            "enabled": true,
            "config": {
                "numberOfIndentationSpaces": 4
            }
        }
    },
    "hints": []
}
"""

        Assert.AreEqual(expectedJson.RemoveWhitepsace(), resultJson.RemoveWhitepsace())
        
    [<Test>]
    member __.``Config specifying hints writes correct JSON document`` () =
        let config = {
            Configuration.formatting = None
            conventions = None
            typography = None
            ignoreFiles = Array.empty
            hints =
                [| "not (a =  b) ===> a <> b"
                   "not (a <> b) ===> a = b" |]
        }
        
        let resultJson = serializeConfig config
        
        let expectedJson =
            """{
    "ignoreFiles": [],
    "hints": [
        "not (a =  b) ===> a <> b",
        "not (a <> b) ===> a = b"
    ]
}
"""

        Assert.AreEqual(expectedJson.RemoveWhitepsace(), resultJson.RemoveWhitepsace())
        
    [<Test>]
    member __.``Load two paths with same root with a common directory; loads expected tree.``() = 
        let expectedLoadedConfigs = 
            { LoadedConfigs = 
                [ (["C:"], None)
                  (["C:"; "Dog"], None)
                  (["C:"; "Dog"; "Goat"], None)
                  (["C:"; "Dog"; "Cat"], None) ] |> Map.ofList
              PathsAdded = [ ["C:"; "Dog"; "Cat"]; ["C:"; "Dog"; "Goat"] ]
              GlobalConfigs = [] }

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
              PathsAdded = [ ["D:"; "Dog"]; ["C:"; "Dog"] ]
              GlobalConfigs = [] }

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
              PathsAdded = [ ["C:"; "Dog"; "Goat"; "Cat"]; ["C:"; "Dog"; "Goat"] ]
              GlobalConfigs = [] }

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
              PathsAdded = [ ["C:"; "Dog"; "Goat"; "Cat"]; ["C:"; "Dog"; "Goat"] ]
              GlobalConfigs = [] }

        let updatedLoadedConfigs = 
            Management.removePath loadedConfigs [ "C:"; "Dog"; "Goat"; "Cat" ]

        let expectedLoadedConfigs = 
            { LoadedConfigs = 
                [ (["C:"], None)
                  (["C:"; "Dog"], None)
                  (["C:"; "Dog"; "Goat"], None) ] |> Map.ofList
              PathsAdded = [ ["C:"; "Dog"; "Goat"] ]
              GlobalConfigs = [] }

        Assert.AreEqual(expectedLoadedConfigs, updatedLoadedConfigs)

        let updatedLoadedConfigs = 
            Management.removePath updatedLoadedConfigs [ "C:"; "Dog"; "Goat" ]

        let expectedLoadedConfigs = 
            { LoadedConfigs = [] |> Map.ofList
              PathsAdded = []
              GlobalConfigs = [] }

        Assert.AreEqual(expectedLoadedConfigs, updatedLoadedConfigs)

    [<Test>]
    member __.``Deepest common path is found when preferred path not found``() = 
        let loadedConfigs = 
            { LoadedConfigs = [] |> Map.ofList
              PathsAdded = 
                [ ["C:"; "Dog"; "Goat"; "Cat"]
                  ["C:"; "Dog"; "Goat"]
                  ["C:"; "Dog"; "Goat"; "Shrimp"] ]
              GlobalConfigs = [] }

        let path = commonPath loadedConfigs ["C:";"NonExistant"]

        Assert.AreEqual(Some(["C:"; "Dog"; "Goat"]), path)

    [<Test>]
    member __.``Preferred path is returned when it is a common path``() = 
        let loadedConfigs = 
            { LoadedConfigs = [] |> Map.ofList
              PathsAdded = 
                [ ["C:"; "Dog"; "Goat"; "Cat"]
                  ["C:"; "Dog"; "Goat"]
                  ["C:"; "Dog"; "Goat"; "Shrimp"] ]
              GlobalConfigs = [] }

        let path = commonPath loadedConfigs ["C:";"Dog"]

        Assert.AreEqual(Some(["C:"; "Dog"]), path)

    [<Test>]
    member __.``No path is returned when there is no common path``() = 
        let loadedConfigs = 
            { LoadedConfigs = [] |> Map.ofList
              PathsAdded = 
                [ ["C:"; "Dog"; "Goat"; "Cat"]
                  ["D:"; "Dog"; "Goat"; "Shrimp"] ]
              GlobalConfigs = [] }

        let path = commonPath loadedConfigs ["C:";"Dog"]

        Assert.AreEqual(None, path)

    [<Test>]
    member __.``No path is returned when there are no paths added``() = 
        let loadedConfigs = 
            { LoadedConfigs = [] |> Map.ofList
              PathsAdded = []
              GlobalConfigs = [] }

        let path = commonPath loadedConfigs ["C:";"Dog"]

        Assert.AreEqual(None, path)

    [<Test>]
    member __.``Default configuration returned when there are no paths added``() = 
        let loadedConfigs = 
            { LoadedConfigs = [] |> Map.ofList
              PathsAdded = []
              GlobalConfigs = [] }

        let config = getConfig loadedConfigs ["C:";"Dog"]

        Assert.AreEqual(Some defaultConfiguration, config)

    [<Test>]
    member __.``Overridden global configuration returned when a global path has been added``() = 
        let loadedConfig = {
            Configuration.formatting = None
            conventions = None
            typography =
                Some { TypographyConfig.indentation = Some { RuleConfig.enabled = false; config = None }
                       maxCharactersOnLine = None
                       trailingWhitespaceOnLine = None
                       maxLinesInFile = None
                       trailingNewLineInFile = None
                       noTabCharacters = None
                }
            ignoreFiles = Array.empty
            hints = Array.empty
        }

        let loadedConfigs = 
            { LoadedConfigs = Map.empty
              PathsAdded = []
              GlobalConfigs = [{ Path = ["C:";"User";".config"]; Name = "User Wide"; Configuration = Some(loadedConfig) }] }

        let config = getConfig loadedConfigs ["C:";"User";".config"]

        let expectedConfig = overrideConfiguration defaultConfiguration loadedConfig

        Assert.AreEqual(Some expectedConfig, config)

    [<Test>]
    member __.``Overridden configuration returned when there is a path added``() = 
        let loadedConfig = {
            Configuration.formatting = None
            conventions = None
            typography =
                Some { TypographyConfig.indentation = Some { RuleConfig.enabled = false; config = None }
                       maxCharactersOnLine = None
                       trailingWhitespaceOnLine = None
                       maxLinesInFile = None
                       trailingNewLineInFile = None
                       noTabCharacters = None
                }
            ignoreFiles = Array.empty
            hints = Array.empty
        }

        let loadedConfigs = 
            { LoadedConfigs = [(["C:"], Some(loadedConfig))] |> Map.ofList
              PathsAdded = [["C:"]]
              GlobalConfigs = [] }

        let config = getConfig loadedConfigs ["C:";"Dog"]

        let expectedConfig = overrideConfiguration defaultConfiguration loadedConfig

        Assert.AreEqual(Some expectedConfig, config)

    [<Test>]
    member __.``Overridden configuration overrides global config``() =
        let globalConfig = {
            Configuration.formatting = None
            conventions = None
            typography =
                Some { TypographyConfig.indentation = Some { RuleConfig.enabled = false; config = None }
                       maxCharactersOnLine = None
                       trailingWhitespaceOnLine = None
                       maxLinesInFile = None
                       trailingNewLineInFile = None
                       noTabCharacters = None
                }
            ignoreFiles = Array.empty
            hints = Array.empty
        }

        let loadedConfig = {
            Configuration.formatting = None
            conventions = None
            typography =
                Some { TypographyConfig.indentation = Some { RuleConfig.enabled = true; config = None }
                       maxCharactersOnLine = None
                       trailingWhitespaceOnLine = None
                       maxLinesInFile = None
                       trailingNewLineInFile = None
                       noTabCharacters = None
                }
            ignoreFiles = Array.empty
            hints = Array.empty
        }

        let loadedConfigs = 
            { LoadedConfigs = [(["C:"], Some(loadedConfig))] |> Map.ofList
              PathsAdded = [["C:"]]
              GlobalConfigs = [{ Path = ["C:";"User";".config"]; Name = "User Wide"; Configuration = Some(globalConfig) }] }

        let config = getConfig loadedConfigs ["C:";"Dog"]

        let overridenGlobalConfig = overrideConfiguration defaultConfiguration globalConfig
        let expectedConfig = overrideConfiguration overridenGlobalConfig loadedConfig

        Assert.AreEqual(Some expectedConfig, config)

    [<Test>]
    member __.``Config override works correctly``() =
        let configToOverride = {
            Configuration.formatting = None
            conventions = None
            typography =
                Some { TypographyConfig.indentation = Some { RuleConfig.enabled = true; config = None }
                       maxCharactersOnLine = Some { RuleConfig.enabled = true; config = None }
                       trailingWhitespaceOnLine = None
                       maxLinesInFile = None
                       trailingNewLineInFile = None
                       noTabCharacters = None
                }
            ignoreFiles = Array.empty
            hints = Array.empty
        }
            
        let overridingConfig = {
            Configuration.formatting = None
            conventions = None
            typography =
                Some { TypographyConfig.indentation = Some { RuleConfig.enabled = false; config = None }
                       maxCharactersOnLine = Some { RuleConfig.enabled = false; config = None }
                       trailingWhitespaceOnLine = None
                       maxLinesInFile = None
                       trailingNewLineInFile = None
                       noTabCharacters = None
                }
            ignoreFiles = Array.empty
            hints = Array.empty
        }
        
        let expectedConfig = {
            Configuration.formatting = None
            conventions = None
            typography =
                Some { TypographyConfig.indentation = Some { RuleConfig.enabled = false; config = None }
                       maxCharactersOnLine = Some { RuleConfig.enabled = false; config = None }
                       trailingWhitespaceOnLine = None
                       maxLinesInFile = None
                       trailingNewLineInFile = None
                       noTabCharacters = None
                }
            ignoreFiles = Array.empty
            hints = Array.empty
        }           
        
        Assert.AreEqual(expectedConfig, overrideConfiguration configToOverride overridingConfig)
