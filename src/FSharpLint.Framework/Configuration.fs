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

                /// Unparsed value from the configuration XML file.
                /// Stored so it can be written back out to a file.
                Content: string
            }

        let private parseIgnoreFiles (ignoreFiles:XElement) =
            let updateAttribute = ignoreFiles.Attributes() |> Seq.tryFind (fun x -> x.Name.LocalName = "Update")

            {
                Files = ignoreFiles.Value.Trim() |> parseLines |> Seq.map parseIgnorePath |> Seq.toList
                Update = 
                    match updateAttribute with
                        | Some(attribute) when attribute.Value.ToUpperInvariant() = "ADD" -> Add
                        | Some(_) | None -> Overwrite
                Content = ignoreFiles.Value
            }

        let getIgnorePathsFromConfig (configRoot:XElement) =
            match configRoot.ElementByLocalName("IgnoreFiles") with
                | Some(ignoreFilesElement) -> parseIgnoreFiles ignoreFilesElement |> Some
                | None -> None

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
            UseTypeChecker: bool option
            IgnoreFiles: IgnoreFiles.IgnoreFilesConfig option
            Analysers: Map<string, Analyser>
        }

        member private this.AnalysersToXml() = 
            let analyserToXml (analyser:System.Collections.Generic.KeyValuePair<string, Analyser>) =
                analyser.Value.ToXml(analyser.Key)

            this.Analysers |> Seq.map analyserToXml |> Seq.toArray

        member this.ToXmlDocument() =
            let content =
                [|
                    match this.IgnoreFiles with 
                        | Some({ Content = content; Update = updateType }) -> 
                            let value = 
                                match updateType with 
                                    | IgnoreFiles.Add -> "Add" 
                                    | IgnoreFiles.Overwrite -> "Overwrite"

                            let attr = XAttribute(XName.op_Implicit "Update", value)
                            yield XElement(getName "IgnoreFiles", XCData(content), attr)
                        | None -> ()

                    match this.UseTypeChecker with 
                        | Some(useTypeChecker) -> 
                            yield XElement(getName "UseTypeChecker", useTypeChecker.ToString())
                        | None -> ()

                    yield XElement(getName "Analysers", this.AnalysersToXml())
                |]

            XDocument(
                XElement(
                    getName "FSharpLintSettings", 
                    content
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
            | None -> None
            | Some(element) -> element.Value.ToUpperInvariant() = "TRUE" |> Some
        
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
        {
            UseTypeChecker = configToOverrideWith.UseTypeChecker

            IgnoreFiles = 
                match configToOverrideWith.IgnoreFiles with
                    | Some({ Update = IgnoreFiles.Overwrite }) -> 
                        configToOverrideWith.IgnoreFiles 
                    | Some({ Update = IgnoreFiles.Add } as newIgnore) -> 
                        let combinedFiles = 
                            match configToOverride.IgnoreFiles with
                                | Some(previousIgnore) ->
                                    newIgnore.Files @ previousIgnore.Files
                                | None -> newIgnore.Files

                        { newIgnore with Files = combinedFiles } |> Some
                    | None ->
                        configToOverride.IgnoreFiles

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

    /// Module to manage the loading and updating of configuration files.
    /// Keeps loaded configurations cached in memory so they can be quickly retrieved.
    module Management =

        open System.IO

        /// Represents a segment within a file system path.
        /// A configuration is loaded for each segment in a path
        /// so that a configuration at the end of the path being loaded
        /// can override previous configuration files.
        type PathNode =
            {
                /// Cached configuration file found at this segment in the path.
                /// `None` when there is no configuration file found at this segment.
                Configuration: Configuration option

                /// Segment this node represents in a path.
                /// e.g. it could be `SomeProj` in `C:\SomeSolution\SomeProj\`
                Segment: string

                /// Nodes located a directory down from this node.
                /// e.g. if this node represents `C:\SomeSolution\SomeProj\`
                /// the children could be `C:\SomeSolution\SomeProj\SomeDir\`
                /// and `C:\SomeSolution\SomeProj\SomeDir2\`.
                Children: PathNode list
            }

        type LoadedConfigs =
            {
                RootPaths: PathNode list
            }

        let private stringToPath (path:string) = 
            path.Split(Path.DirectorySeparatorChar)

        let listEndsWith containingList listToCheck = 
            let minListLength = List.length listToCheck

            let rec endsWith = function
            | (_::rest) as subList -> 
                if subList = listToCheck then true
                else endsWith rest
            | x when List.length x < minListLength -> false
            | [] -> false

            endsWith containingList

        let private pathToString path =
            path |> String.concat (Path.DirectorySeparatorChar.ToString())

        let private loadConfigForPath tryLoadConfig (currentPath:string list) (path:string array) = 
            let paths = 
                [ for i in currentPath.Length..path.Length - 1 -> 
                    let path, segment = Array.toList path.[..i], path.[i]
                    (path, segment) ]

            let rec loadPaths = function
            | (path, segment)::rest ->
                [{ 
                    Configuration = tryLoadConfig path
                    Segment = segment
                    Children = loadPaths rest }]
            | [] -> []

            loadPaths paths

        let addPath tryLoadConfig node path = 
            let path = stringToPath path

            let pathEndsWith = listEndsWith (path |> Array.rev |> Array.toList)

            let rec copyNode node currentPath = 
                let currentPath = node.Segment::currentPath

                let childBreaksPath child = 
                    (pathEndsWith >> not) (child.Segment::currentPath)

                let children = 
                    [ for child in node.Children do yield copyNode child currentPath ]

                { node with 
                    Children = 
                        if pathEndsWith currentPath && node.Children |> List.forall childBreaksPath then
                            [ yield! children; yield! loadConfigForPath tryLoadConfig currentPath path ]
                        else
                            children }

            let copiedRoots = [ for root in node.RootPaths -> copyNode root [] ]

            { RootPaths = 
                [ yield! copiedRoots
                  if node.RootPaths |> List.forall (fun x -> (pathEndsWith >> not) [x.Segment]) then 
                    yield! loadConfigForPath tryLoadConfig [] path ] }

        let removePath node path = 
            let path = stringToPath path |> Array.rev

            let pathList = Array.toList path

            let pathStartsWith = listEndsWith pathList

            let rec nodeShouldBeRemoved currentPath node = 
                let currentPath = node.Segment::currentPath

                if currentPath = pathList then
                    true
                else
                    match node.Children with
                    | [child] -> nodeShouldBeRemoved currentPath child
                    | _ -> false

            let rec copyNode node currentPath = 
                let currentPath = node.Segment::currentPath

                { node with 
                    Children = 
                        [ for child in node.Children do 
                            if not <| nodeShouldBeRemoved currentPath child then
                                yield copyNode child currentPath ]
                }

            if nodeShouldBeRemoved [] node then
                None
            else
                copyNode node [] |> Some

        /// Attempts to get a path that is common to all paths
        /// that have been added to a node.
        /// If a given preferred path is found to be a common path then
        /// that path will always be returned, useful if you want to prefer
        /// the solution directory for example.
        let commonPath node preferredPath = 
            let rec getCommonDirs node currentPath = 
                let currentPath = node.Segment::currentPath

                match node.Children with 
                | [commonDir] when currentPath <> preferredPath -> 
                    node::(getCommonDirs commonDir currentPath)
                | _ -> [node]

            match node.RootPaths with
            | [commonRoot] ->
                match getCommonDirs commonRoot [] with
                | [commonDirectory] -> Some(commonDirectory)
                | _ -> None
            | _ -> None            

        /// Tries to reload the configuration for all paths.
        /// Call when the user has edited a configuration file on disk.
        let refresh tryLoadConfig node = 
            let rec copyNode node currentPath = 
                let currentPath = node.Segment::currentPath

                { node with 
                    Configuration = tryLoadConfig copyNode
                    Children = 
                        [ for child in node.Children do yield copyNode child currentPath ]
                }

            { RootPaths = [ for root in node.RootPaths -> copyNode root [] ] }

        /// Gets the configuration file located at a given path.
        /// The configuration file returned may be incomplete as it
        /// will not have overrided any previous configuration files.
        let partialConfig node path = 
            let path = stringToPath path |> Array.toList
        
            let rec find node = function
            | currentSegment::nextSegment::unmatchedPathSegments when currentSegment = node.Segment ->
                let matchingChild =
                    node.Children 
                        |> List.tryFind (fun x -> x.Segment = nextSegment)

                match matchingChild with
                | Some(child) -> find child unmatchedPathSegments
                | None -> None
            | currentSegment::[] when currentSegment = node.Segment ->
                node.Configuration
            | _ -> None

            find node path

        let private tryOverrideConfig = function
        | Some(configToOverride), Some(config) -> 
            Some(overrideConfiguration configToOverride config)
        | Some(x), None
        | None, Some(x) -> Some(x)
        | None, None -> None
        
        /// Gets the complete configuration file located at a given path.
        /// "complete" configuration means that it has overridden any previous
        /// configuration files.
        let config node path = 
            let path = stringToPath path |> Array.toList
        
            let rec find node configSoFar = function
            | currentSegment::nextSegment::unmatchedPathSegments when currentSegment = node.Segment ->
                let matchingChild =
                    node.Children 
                        |> List.tryFind (fun x -> x.Segment = nextSegment)

                match matchingChild with
                | Some(child) -> 
                    let config = tryOverrideConfig (configSoFar, node.Configuration)
                    find child config unmatchedPathSegments
                | None -> None
            | currentSegment::[] when currentSegment = node.Segment ->
                tryOverrideConfig (configSoFar, node.Configuration)
            | _ -> None

            find node None path

        /// Gets all paths that have currently been loaded so far.
        let getPaths node = 
            let rec getPaths node currentPath = 
                let currentPath = node.Segment::currentPath

                currentPath::[ for child in node.Children do yield! getPaths child currentPath ]

            [ for root in node.RootPaths do 
                yield! getPaths root []|> List.map (List.rev >> String.concat (Path.DirectorySeparatorChar.ToString())) ]

        /// Updates a configuration file at a given path.
        let updateConfig node path config = 
            let path = stringToPath path |> Array.toList

            let rec copyNode node currentPath = 
                let currentPath = node.Segment::currentPath

                { node with 
                    Configuration = 
                        if path = currentPath then Some(config)
                        else node.Configuration
                    Children = 
                        [ for child in node.Children do yield copyNode child currentPath ]
                }

            copyNode node []