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

namespace FSharpLint.Framework

/// Loads configuration files from xml into an object.
/// When a configuration file has already been loaded, loading another one overwrites the existing configuration.
/// The overwrite works by only changing existing properties with properties from the new file, 
/// so properties in the original configuration file not in the new configuration file will remain.
module Configuration =

    open System.Xml.Linq

    [<Literal>]
    let private Namespace = @"https://github.com/fsprojects/FSharpLint/blob/master/ConfigurationSchema.xsd"

    let private getName name =
        XName.Get(name, Namespace)

    type XElement with
        member this.ElementByLocalName localName =
            this.Elements() 
                |> Seq.tryFind (fun x -> x.Name.LocalName = localName)

    type Access =
        | Public = 0
        | Internal = 1
        | Private = 2
        | None = 3
        | All = 4
        | NotPrivate = 5
        | NotPublic = 6

    exception ConfigurationException of string

    open Microsoft.FSharp.Reflection

    type Setting =
        | Enabled of bool
        | Lines of int
        | Depth of int
        | MaxItems of int
        | Length of int
        | Hints of string list
        | MaxCyclomaticComplexity of int
        | IncludeMatchStatements of bool
        | OneSpaceAllowedAfterOperator of bool
        | NumberOfSpacesAllowed of int
        | IgnoreBlankLines of bool
        | Access of Access

    let private settingToXml = function
        | Lines(x)
        | Depth(x)
        | MaxItems(x)
        | MaxCyclomaticComplexity(x)
        | Length(x)
        | NumberOfSpacesAllowed(x) -> x :> obj
        | IncludeMatchStatements(x)
        | OneSpaceAllowedAfterOperator(x)
        | Enabled(x)
        | IgnoreBlankLines(x) -> x.ToString() :> obj
        | Access(x) -> x :> obj
        | Hints(x) -> 
            String.concat System.Environment.NewLine x 
                |> (fun x -> XCData(x)) :> obj

    let private settingsToXml (settings:Map<string, Setting>) =
        settings
            |> Seq.map (fun x -> XElement(getName x.Key, settingToXml x.Value) :> obj)
            |> Seq.toArray

    let private parseLines (content:string) =
        content.Split('\n') 
            |> Seq.map (fun x -> x.Trim()) 
            |> Seq.filter (System.String.IsNullOrWhiteSpace >> not) 
            |> Seq.toList

    module IgnoreFiles =

        open System
        open System.IO
        open System.Text.RegularExpressions

        type IsDirectory = | IsDirectory of bool

        type Ignore =
            | Ignore of Regex list * IsDirectory
            | Negate of Regex list * IsDirectory

        let parseIgnorePath (path:string) = 
            let globToRegex glob =
                Regex(
                    "^" + Regex.Escape(glob).Replace(@"\*", ".*").Replace(@"\?", ".") + "$",
                    RegexOptions.IgnoreCase ||| RegexOptions.Singleline)

            let isDirectory = path.EndsWith("/")

            let getRegexSegments (path:string) = 
                path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) 
                    |> Array.map globToRegex

            if path.StartsWith("!") then
                getRegexSegments (path.Substring(1))
                    |> Array.toList
                    |> fun segments -> Negate(segments, IsDirectory(isDirectory))
            else
                getRegexSegments (if path.StartsWith(@"\!") then path.Substring(1) else path)
                    |> Array.toList
                    |> fun segments -> Ignore(segments, IsDirectory(isDirectory))

        let private pathMatchesGlob (globs:Regex list) (path:string list) isDirectory = 
            let rec getRemainingGlobSeqForMatches pathSegment (globSeqs:Regex list list) = 
                globSeqs |> List.choose (function
                    | globSegment::remaining when globSegment.IsMatch(pathSegment) -> Some remaining
                    | x -> None)

            let rec doesGlobSeqMatchPathSeq remainingPath currentlyMatchingGlobs = 
                match remainingPath with
                    | [currentSegment] when isDirectory -> false 
                    | currentSegment::remaining -> 
                        let currentlyMatchingGlobs = globs::currentlyMatchingGlobs

                        let currentlyMatchingGlobs = getRemainingGlobSeqForMatches currentSegment currentlyMatchingGlobs

                        let aGlobWasCompletelyMatched = currentlyMatchingGlobs |> List.exists List.isEmpty

                        let matched = aGlobWasCompletelyMatched && (isDirectory || (not isDirectory && List.isEmpty remaining))

                        if matched then true
                        else doesGlobSeqMatchPathSeq remaining currentlyMatchingGlobs
                    | [] -> false

            doesGlobSeqMatchPathSeq path []
            
        let shouldFileBeIgnored (ignorePaths:Ignore list) (filePath:string) = 
            let segments = filePath.Split Path.DirectorySeparatorChar |> Array.toList

            ignorePaths |> List.fold (fun isCurrentlyIgnored ignoreGlob -> 
                match ignoreGlob with
                    | Ignore(glob, IsDirectory(isDirectory)) 
                        when not isCurrentlyIgnored && pathMatchesGlob glob segments isDirectory -> true
                    | Negate(glob, IsDirectory(isDirectory))
                        when isCurrentlyIgnored && pathMatchesGlob glob segments isDirectory -> false
                    | _ -> isCurrentlyIgnored) false

        type IgnoreFilesUpdate = 
            | Add
            | Overwrite

        type IgnoreFilesConfig =
            {
                Update: IgnoreFilesUpdate
                Files: Ignore list
            }

        let private parseIgnoreFiles (ignoreFiles:XElement) =
            let updateAttribute = ignoreFiles.Attributes() |> Seq.tryFind (fun x -> x.Name.LocalName = "Update")

            {
                Files = ignoreFiles.Value.Trim() |> parseLines |> Seq.map parseIgnorePath |> Seq.toList
                Update = 
                    match updateAttribute with
                        | Some(attribute) when attribute.Value.ToUpperInvariant() = "ADD" -> Add
                        | Some(_) | None -> Overwrite
            }

        let getIgnorePathsFromConfig (configRoot:XElement) =
            match configRoot.ElementByLocalName("IgnoreFiles") with
                | Some(ignoreFilesElement) -> parseIgnoreFiles ignoreFilesElement
                | None -> { Update = Add; Files = [] }

    type Rule =
        {
            Settings: Map<string, Setting>
        }
        
    /// An analyser groups together related rules in the configuration file.
    type Analyser =
        {
            Settings: Map<string, Setting>
            Rules: Map<string, Rule>
        }

        member this.ToXml(name) =
            let rulesContent =
                this.Rules 
                    |> Seq.map (fun x -> 
                        let settingsXml = settingsToXml x.Value.Settings
                        XElement(getName x.Key, settingsXml) :> obj)
                    |> Seq.toArray

            let content = 
                [| 
                    yield XElement(getName "Rules", rulesContent) :> obj
                    yield! settingsToXml this.Settings
                |]

            XElement(getName name, content) :> obj

    type Configuration =
        {
            UseTypeChecker: bool
            IgnoreFiles: IgnoreFiles.IgnoreFilesConfig
            Analysers: Map<string, Analyser>
        }

        member private this.IgnoreFilesToXml() = "" // TODO: store string representation in ignorefiles record

        member private this.AnalysersToXml() = 
            let analyserToXml (analyser:System.Collections.Generic.KeyValuePair<string, Analyser>) =
                analyser.Value.ToXml(analyser.Key)

            this.Analysers |> Seq.map analyserToXml |> Seq.toArray

        member this.ToXmlDocument() =
            XDocument(
                XElement(
                    getName "FSharpLintSettings", 
                    XElement(getName "UseTypeChecker", this.UseTypeChecker.ToString()),
                    XElement(getName "IgnoreFiles", this.IgnoreFilesToXml()),
                    XElement(getName "Analysers", this.AnalysersToXml())
                    )
                )

    let private toAccess value =
        let (valid, ret) = System.Enum.TryParse(value)
        if not valid then sprintf "Found unknown XmlDocumentation Access value %s" value |> ConfigurationException |> raise
        ret

    let private parseSetting (setting:XElement) =
        match setting.Name.LocalName with
            | "Enabled" -> Enabled(setting.Value |> bool.Parse)
            | "Lines" -> Lines(setting.Value |> int)
            | "Depth" -> Depth(setting.Value |> int)
            | "Length" -> Length(setting.Value |> int)
            | "MaxItems" -> MaxItems(setting.Value |> int)
            | "MaxCyclomaticComplexity" -> MaxCyclomaticComplexity(setting.Value |> int)
            | "IncludeMatchStatements" -> IncludeMatchStatements(setting.Value |> bool.Parse)
            | "Hints" -> Hints(parseLines setting.Value)
            | "OneSpaceAllowedAfterOperator" -> OneSpaceAllowedAfterOperator(setting.Value |> bool.Parse)
            | "NumberOfSpacesAllowed" -> NumberOfSpacesAllowed(setting.Value |> int)
            | "IgnoreBlankLines" -> IgnoreBlankLines(setting.Value |> bool.Parse)
            | "Access" -> Access(setting.Value |> toAccess)
            | settingName ->
                sprintf "Found unknown setting %s" settingName |> ConfigurationException |> raise

    let toSetting (settingElement:XElement) = (settingElement.Name.LocalName, parseSetting settingElement)

    let parseRule (rule:XElement) : Rule =
        { 
            Settings = rule.Elements() |> Seq.map toSetting |> Map.ofSeq
        }

    let parseAnalyser (analyser:XElement) =
        let toRule (ruleElement:XElement) = (ruleElement.Name.LocalName, parseRule ruleElement)

        let analyserDetails =
            {
                Settings = analyser.Elements() 
                    |> Seq.filter (fun x -> x.Name.LocalName <> "Rules") 
                    |> Seq.map toSetting
                    |> Map.ofSeq

                Rules = 
                    match analyser.ElementByLocalName("Rules") with
                        | Some(rulesElement) -> rulesElement.Elements() |> Seq.map toRule |> Map.ofSeq
                        | None -> Map.empty
            }

        (analyser.Name.LocalName, analyserDetails)

    let private getUseTypeChecker (config:XElement) =
        match config.ElementByLocalName("UseTypeChecker") with
            | None -> false
            | Some(element) -> element.Value.ToUpperInvariant() = "TRUE"
        
    /// Parse a configuration file.
    let configuration (file:string) = 
        use configReader = new System.IO.StringReader(file)
        let config = XDocument.Load(configReader).Root

        {
            UseTypeChecker = getUseTypeChecker config

            IgnoreFiles = IgnoreFiles.getIgnorePathsFromConfig config

            Analysers = 
                match config.ElementByLocalName("Analysers") with
                    | Some(analysers) -> analysers.Elements() |> Seq.map parseAnalyser |> Map.ofSeq
                    | None -> Map.empty
        }

    let overwriteMap (oldMap:Map<'a,'b>) (newMap:Map<'a,'b>) overwriteValue =
        [ 
            for keyValuePair in oldMap do
                if newMap |> Map.containsKey keyValuePair.Key then
                    yield (keyValuePair.Key, overwriteValue keyValuePair.Value newMap.[keyValuePair.Key])
                else
                    yield (keyValuePair.Key, keyValuePair.Value)
        ]
            |> Map.ofList

    let private overrideRuleSettings oldProperty newProperty = newProperty

    let private overrideRule (oldRule:Rule) (newRule:Rule) : Rule =
        { 
            Settings = overwriteMap oldRule.Settings newRule.Settings overrideRuleSettings 
        }

    let private overrideAnalysers oldRules newRules =
        { 
            Rules = overwriteMap oldRules.Rules newRules.Rules overrideRule 
            Settings = overwriteMap oldRules.Settings newRules.Settings overrideRuleSettings
        }

    /// <summary>
    /// Loads a "higher precedence" configuration file. All the properties in the file we're loading overwrite 
    /// the same properties in our previous configuration with the new values, any properties that don't exist 
    /// in the previous configuration are added, and any properties that don't exist in the configuration being 
    /// loaded are left alone.
    /// </summary>
    /// <param name="file">Path of the configuration file that will override the existing configuration</param>
    let overrideConfiguration configToOverride configToOverrideWith =
        let combineIgnoreFiles () = List.concat [configToOverrideWith.IgnoreFiles.Files; configToOverride.IgnoreFiles.Files]

        {
            UseTypeChecker = configToOverrideWith.UseTypeChecker

            IgnoreFiles = 
                match configToOverrideWith.IgnoreFiles.Update with
                    | IgnoreFiles.Overwrite -> configToOverrideWith.IgnoreFiles 
                    | IgnoreFiles.Add -> { configToOverrideWith.IgnoreFiles with Files = combineIgnoreFiles() }

            Analysers = overwriteMap configToOverride.Analysers configToOverrideWith.Analysers overrideAnalysers
        }

    /// A default configuration specifying every analyser and rule is included as a resource file in the framework.
    /// This function loads and returns this default configuration.
    let defaultConfiguration =
        let assembly = System.Reflection.Assembly.GetExecutingAssembly()
        let resourceName = "DefaultConfiguration.FSharpLint"

        use stream = assembly.GetManifestResourceStream(resourceName)
        use reader = new System.IO.StreamReader(stream)

        reader.ReadToEnd() |> configuration

    /// Checks if a analyser in the configuration is enabled.
    /// Returns the analyser settings if the analyser was enabled; None otherwise.
    let isAnalyserEnabled config analyserName =
        if not <| config.Analysers.ContainsKey analyserName then
            sprintf "Expected %s analyser in config." analyserName
                |> ConfigurationException
                |> raise

        let analyserSettings = config.Analysers.[analyserName].Settings

        if analyserSettings.ContainsKey "Enabled" then
            match analyserSettings.["Enabled"] with 
                | Enabled(true) -> Some(analyserSettings)
                | _ -> None
        else
            Some(analyserSettings)

    /// Checks if a rule in the configuration is enabled and the analyser it's within is also enabled.
    /// Returns the analyser settings and rule settings if the rule was enabled; None otherwise.
    let isRuleEnabled config analyserName ruleName =
        match isAnalyserEnabled config analyserName with
            | Some(analyserSettings) ->
                let rules = config.Analysers.[analyserName].Rules

                if not <| rules.ContainsKey ruleName then 
                    sprintf "Expected rule %s for %s analyser in config." ruleName analyserName
                        |> ConfigurationException
                        |> raise

                let ruleSettings = rules.[ruleName].Settings

                if ruleSettings.ContainsKey "Enabled" then
                    match ruleSettings.["Enabled"] with 
                        | Enabled(true) -> Some(analyserSettings, ruleSettings)
                        | _ -> None
                else
                    None
            | None -> None