// FSharpLint, a linter for F#.
// Copyright (C) 2016 Matthew Mcveigh
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace FSharpLint.Framework

/// Loads configuration files from xml into an object.
/// When a configuration file has already been loaded, loading another one overwrites the existing configuration.
/// The overwrite works by only changing existing properties with properties from the new file,
/// so properties in the original configuration file not in the new configuration file will remain.
module Configuration =

    open System.Reflection
    open System.Xml.Linq

    [<Literal>]
    let SettingsFileName = "Settings.FSharpLint"

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

    type Naming =
        | PascalCase = 0
        | CamelCase = 1

    type Hint = { Hint: string; ParsedHint: HintParser.Hint }

    type Setting =
        | Enabled of bool
        | Lines of int
        | Depth of int
        | MaxItems of int
        | Length of int
        | Hints of Hint list
        | OneSpaceAllowedAfterOperator of bool
        | NumberOfSpacesAllowed of int
        | IgnoreBlankLines of bool
        | Access of Access
        | Naming of Naming
        | Prefix of string
        | Suffix of string
        | Underscores of bool

    let private settingToXml = function
        | Lines(x)
        | Depth(x)
        | MaxItems(x)
        | Length(x)
        | NumberOfSpacesAllowed(x) -> x :> obj
        | Prefix(x)
        | Suffix(x) -> x :> obj
        | OneSpaceAllowedAfterOperator(x)
        | Enabled(x)
        | Underscores(x)
        | IgnoreBlankLines(x) -> x.ToString() :> obj
        | Access(x) -> x :> obj
        | Naming(x) -> x :> obj
        | Hints(hints) ->
            hints
            |> List.map (fun x -> x.Hint)
            |> String.concat System.Environment.NewLine
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

        [<NoComparison>]
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
                    | _ -> None)

            let rec doesGlobSeqMatchPathSeq remainingPath currentlyMatchingGlobs =
                match remainingPath with
                | [_] when isDirectory -> false
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

        [<NoComparison>]
        type IgnoreFilesConfig =
            { Update: IgnoreFilesUpdate
              Files: Ignore list

              /// Unparsed value from the configuration XML file.
              /// Stored so it can be written back out to a file.
              Content: string }

        let private parseIgnoreFiles (ignoreFiles:XElement) =
            let updateAttribute = ignoreFiles.Attributes() |> Seq.tryFind (fun x -> x.Name.LocalName = "Update")

            { Files = ignoreFiles.Value.Trim() |> parseLines |> Seq.map parseIgnorePath |> Seq.toList
              Update =
                match updateAttribute with
                | Some(attribute) when attribute.Value.ToUpperInvariant() = "ADD" -> Add
                | Some(_) | None -> Overwrite
              Content = ignoreFiles.Value }

        let getIgnorePathsFromConfig (configRoot:XElement) =
            match configRoot.ElementByLocalName("IgnoreFiles") with
            | Some(ignoreFilesElement) -> parseIgnoreFiles ignoreFilesElement |> Some
            | None -> None

    type Rule =  { Settings: Map<string, Setting> }

    /// An analyser groups together related rules in the configuration file.
    type Analyser =
        { Settings: Map<string, Setting>
          Rules: Map<string, Rule> }

        member this.ToXml(name) =
            let rulesContent =
                this.Rules
                    |> Seq.map (fun x ->
                        let settingsXml = settingsToXml x.Value.Settings
                        XElement(getName x.Key, settingsXml) :> obj)
                    |> Seq.toArray

            let content =
                [| yield XElement(getName "Rules", rulesContent) :> obj
                   yield! settingsToXml this.Settings |]

            XElement(getName name, content) :> obj

    [<NoComparison>]
    type Configuration =
        { UseTypeChecker: bool option
          IgnoreFiles: IgnoreFiles.IgnoreFilesConfig option
          Analysers: Map<string, Analyser> }

        member private this.AnalysersToXml() =
            let analyserToXml (analyser:System.Collections.Generic.KeyValuePair<string, Analyser>) =
                analyser.Value.ToXml(analyser.Key)

            this.Analysers |> Seq.map analyserToXml |> Seq.toArray

        member this.ToXmlDocument() =
            let content =
                [| match this.IgnoreFiles with
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

                   yield XElement(getName "Analysers", this.AnalysersToXml()) |]

            XDocument(XElement(getName "FSharpLintSettings", content))

    let private fromEnum name value =
        let (valid, ret) = System.Enum.TryParse(value)
        if not valid then sprintf "Found unknown XmlDocumentation %s value %s" name value |> ConfigurationException |> raise
        ret

    let private parseHints (hintsText:string) =
        let parseHint hint =
            match FParsec.CharParsers.run HintParser.phint hint with
            | FParsec.CharParsers.Success(hint, _, _) -> hint
            | FParsec.CharParsers.Failure(error, _, _) ->
                raise <| ConfigurationException("Failed to parse hint: " + hint + "\n" + error)

        parseLines hintsText
        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
        |> List.map (fun x -> { Hint = x; ParsedHint = parseHint x })

    let private parseSetting (setting:XElement) =
        match setting.Name.LocalName with
        | "Enabled" -> Enabled(setting.Value |> bool.Parse)
        | "Lines" -> Lines(setting.Value |> int)
        | "Depth" -> Depth(setting.Value |> int)
        | "Length" -> Length(setting.Value |> int)
        | "MaxItems" -> MaxItems(setting.Value |> int)
        | "Hints" -> Hints(parseHints setting.Value)
        | "OneSpaceAllowedAfterOperator" -> OneSpaceAllowedAfterOperator(setting.Value |> bool.Parse)
        | "NumberOfSpacesAllowed" -> NumberOfSpacesAllowed(setting.Value |> int)
        | "IgnoreBlankLines" -> IgnoreBlankLines(setting.Value |> bool.Parse)
        | "Access" -> Access(setting.Value |> fromEnum "Access")
        | "Naming" -> Naming(setting.Value |> fromEnum "Naming")
        | "Prefix" -> Prefix(setting.Value)
        | "Suffix" -> Suffix(setting.Value)
        | "Underscores" -> Underscores(setting.Value |> bool.Parse)
        | settingName ->
            sprintf "Found unknown setting %s" settingName |> ConfigurationException |> raise

    let toSetting (settingElement:XElement) = (settingElement.Name.LocalName, parseSetting settingElement)

    let parseRule (rule:XElement) : Rule =
        { Settings = rule.Elements() |> Seq.map toSetting |> Map.ofSeq }

    let parseAnalyser (analyser:XElement) =
        let toRule (ruleElement:XElement) = (ruleElement.Name.LocalName, parseRule ruleElement)

        let analyserDetails =
            { Settings = analyser.Elements()
                |> Seq.filter (fun x -> x.Name.LocalName <> "Rules")
                |> Seq.map toSetting
                |> Map.ofSeq
              Rules =
                match analyser.ElementByLocalName("Rules") with
                | Some(rulesElement) -> rulesElement.Elements() |> Seq.map toRule |> Map.ofSeq
                | None -> Map.empty }

        (analyser.Name.LocalName, analyserDetails)

    let private getUseTypeChecker (config:XElement) =
        match config.ElementByLocalName("UseTypeChecker") with
        | None -> None
        | Some(element) -> element.Value.ToUpperInvariant() = "TRUE" |> Some

    /// Parse a configuration file.
    let configuration (file:string) =
        use configReader = new System.IO.StringReader(file)
        let config = XDocument.Load(configReader).Root

        { UseTypeChecker = getUseTypeChecker config
          IgnoreFiles = IgnoreFiles.getIgnorePathsFromConfig config
          Analysers =
            match config.ElementByLocalName("Analysers") with
            | Some(analysers) -> analysers.Elements() |> Seq.map parseAnalyser |> Map.ofSeq
            | None -> Map.empty }

    let overwriteMap (oldMap:Map<'a,'b>) (newMap:Map<'a,'b>) overwriteValue =
        [ for keyValuePair in oldMap do
            match Map.tryFind keyValuePair.Key newMap with
            | Some(value) -> yield (keyValuePair.Key, overwriteValue keyValuePair.Value value)
            | None -> yield (keyValuePair.Key, keyValuePair.Value) ] |> Map.ofList

    let private overrideRuleSettings _ newProperty = newProperty

    let private overrideRule (oldRule:Rule) (newRule:Rule) : Rule =
        { Settings = overwriteMap oldRule.Settings newRule.Settings overrideRuleSettings }

    let private overrideAnalysers oldRules newRules =
        { Rules = overwriteMap oldRules.Rules newRules.Rules overrideRule
          Settings = overwriteMap oldRules.Settings newRules.Settings overrideRuleSettings }

    /// <summary>
    /// Loads a "higher precedence" configuration file. All the properties in the file we're loading overwrite
    /// the same properties in our previous configuration with the new values, any properties that don't exist
    /// in the previous configuration are added, and any properties that don't exist in the configuration being
    /// loaded are left alone.
    /// </summary>
    /// <param name="file">Path of the configuration file that will override the existing configuration</param>
    let overrideConfiguration configToOverride configToOverrideWith =
        { UseTypeChecker = configToOverrideWith.UseTypeChecker
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
          Analysers = overwriteMap configToOverride.Analysers configToOverrideWith.Analysers overrideAnalysers }

    let private getMapDifferences (map:Map<_, _>) (newMap:Map<_, _>) =
        newMap |> Map.filter (fun key value ->
            match map.TryFind key with
            | Some(x) -> x <> value
            | None -> true)

    let private getAnalyserDifferences analyser newAnalyser =
        { Settings = getMapDifferences analyser.Settings newAnalyser.Settings
          Rules = getMapDifferences analyser.Rules newAnalyser.Rules }

    /// Merges settings from `diff` and `partial` with settings from
    /// `diff` taking precedence.
    let private mergeSettings full diff partial =
        full
        |> Map.toList
        |> List.choose (fun (key, _) ->
            match Map.tryFind key diff with
            | Some(value) -> Some(key, value)
            | None ->
                match Map.tryFind key partial with
                | Some(value) -> Some(key, value)
                | None -> None)
        |> Map.ofList

    /// Merges rules from `diff` and `partial` with rules and settings
    /// within rules from `diff` taking precedence.
    let private mergeRules (full:Map<_, Rule>) (diff:Map<_, Rule>) partial =
        full
        |> Map.toList
        |> List.choose (fun (ruleName, ruleToUpdate) ->
            let findRule = Map.tryFind ruleName

            match (findRule diff, findRule partial) with
            | Some(diff), None -> Some(ruleName, diff)
            | None, Some(partial) -> Some(ruleName, partial)
            | Some(diff), Some(partial) ->
                let rule =
                    { Rule.Settings =
                        mergeSettings ruleToUpdate.Settings
                                        diff.Settings
                                        partial.Settings }
                Some(ruleName, rule)
            | None, None -> None)
        |> Map.ofList

    /// Updates a partial config adding only changes - so only what is needed is added to the config.
    let updateConfigMap fullUpdatedConfig fullConfigToUpdate partialConfigToUpdate =
        let mergeAnalyser updatedAnalyser analyserDiff partialAnalyser =
            { Rules =
                mergeRules updatedAnalyser.Rules
                           analyserDiff.Rules
                           partialAnalyser.Rules
              Settings =
                mergeSettings updatedAnalyser.Settings
                              analyserDiff.Settings
                              partialAnalyser.Settings }

        let updatedAnalysers =
            fullConfigToUpdate.Analysers
            |> Map.toList
            |> List.choose (fun (analyserName, analyserToUpdate) ->
                let updatedAnalyser = fullUpdatedConfig.Analysers.[analyserName]

                let diff = getAnalyserDifferences analyserToUpdate updatedAnalyser

                let noUpdates =
                    diff.Rules.Count = 0 &&
                    diff.Settings.Count = 0

                match partialConfigToUpdate.Analysers.TryFind analyserName with
                | Some(partialAnalyser) ->
                    let analyser = mergeAnalyser updatedAnalyser diff partialAnalyser
                    Some(analyserName, analyser)
                | None when noUpdates -> None
                | None -> Some(analyserName, diff))
            |> Map.ofList

        { fullUpdatedConfig with
            Analysers = updatedAnalysers }

    /// A default configuration specifying every analyser and rule is included as a resource file in the framework.
    /// This function loads and returns this default configuration.
    let defaultConfiguration =
        let assembly = typeof<Configuration>.GetTypeInfo().Assembly
        let resourceName = "DefaultConfiguration.FSharpLint"

        use stream = assembly.GetManifestResourceStream(resourceName)
        use reader = new System.IO.StreamReader(stream)

        reader.ReadToEnd() |> configuration

    /// Checks if a analyser in the configuration is enabled.
    /// Returns the analyser settings if the analyser was enabled; None otherwise.
    let isAnalyserEnabled config analyserName =
        match Map.tryFind analyserName config.Analysers with
        | Some(analyser) ->
            let analyserSettings = analyser.Settings

            match Map.tryFind "Enabled" analyserSettings with
            | Some(Enabled(false)) -> None
            | _ -> Some(analyser)
        | None ->
            sprintf "Expected %s analyser in config." analyserName
            |> ConfigurationException
            |> raise

    /// Checks if a rule in the configuration is enabled and the analyser it's within is also enabled.
    /// Returns the analyser settings and rule settings if the rule was enabled; None otherwise.
    let isRuleEnabled config analyserName ruleName =
        match isAnalyserEnabled config analyserName with
        | Some(analyser) ->
            let rules = analyser.Rules

            match Map.tryFind ruleName rules with
            | Some(rule) ->
                let ruleSettings = rule.Settings

                match Map.tryFind "Enabled" ruleSettings with
                | Some(Enabled(true)) -> Some(analyser.Settings, ruleSettings)
                | _ -> None
            | None ->
                sprintf "Expected rule %s for %s analyser in config." ruleName analyserName
                |> ConfigurationException
                |> raise
        | None -> None

    /// Module to manage the loading and updating of configuration files.
    /// Keeps loaded configurations cached in memory so they can be quickly retrieved.
    module Management =

        open System
        open System.IO

        type Path = string list

        [<NoComparison>]
        type GlobalConfig = { Path: Path; Name: string; Configuration: Configuration option }

        /// Keeps configuration files loaded for a list of paths so that
        /// they can be quickly retrieved and updated
        [<NoComparison>]
        type LoadedConfigs =
            { /// Cached configurations for each path.
              LoadedConfigs: Map<Path, Configuration option>

              /// Full paths added, there could be multiple <see cref="LoadedConfigs.LoadedConfigs" />
              /// for each full path. If you wanted to load the configurations for a solution
              /// this should be a list of absolute paths to the project directories.
              PathsAdded: Path list

              /// Global configuration files in order of precedence.
              /// All PathsAdded will override these files.
              GlobalConfigs: GlobalConfig list }

            static member Empty = { LoadedConfigs = Map.empty; PathsAdded = []; GlobalConfigs = [] }

        let private getAllPaths path =
            let rec getAllPaths = function
                | x::rest, currentPath, pathsFound ->
                    let pathFound = currentPath@[x]
                    getAllPaths (rest, pathFound, pathFound::pathsFound)
                | [], _, pathsFound -> pathsFound

            getAllPaths (path, [], []) |> List.rev

        /// Loads all configurations needed to form a complete configuration for a given path.
        /// A `complete configuration` is one that has overridden every configuration file in ancestor directories.
        let addPath tryLoadConfig loadedConfigs path =
            let pathHasAlreadyBeenLoaded =
                loadedConfigs.PathsAdded |> List.exists (fun x -> x = path)

            if pathHasAlreadyBeenLoaded then loadedConfigs
            else
                let paths = getAllPaths path

                let rec updateLoadedConfigs loadedConfigs = function
                | path::rest ->
                    if loadedConfigs |> Map.containsKey path then
                        updateLoadedConfigs loadedConfigs rest
                    else
                        let updatedLoadedConfigs =
                            loadedConfigs |> Map.add path (tryLoadConfig path)

                        updateLoadedConfigs updatedLoadedConfigs rest
                | [] -> loadedConfigs

                { loadedConfigs with
                    PathsAdded = path::loadedConfigs.PathsAdded
                    LoadedConfigs = updateLoadedConfigs loadedConfigs.LoadedConfigs paths }

        let rec private listStartsWith = function
        | (_, []) -> true
        | (x::list, y::startsWithList) when x = y ->
            listStartsWith (list, startsWithList)
        | _ -> false

        let private isPathPartOfAnyPaths path paths =
            paths |> List.exists (fun x -> listStartsWith (x, path))

        /// Removes a loaded path and all cached configurations that aren't used by any other paths.
        let removePath loadedConfigs path =
            let pathNeverLoaded =
                loadedConfigs.PathsAdded |> List.exists (fun x -> x = path) |> not

            if pathNeverLoaded then
                loadedConfigs
            else
                let updatedPaths =
                    loadedConfigs.PathsAdded |> List.filter (fun x -> x <> path)

                let updatedConfigs =
                    loadedConfigs.LoadedConfigs
                    |> Map.filter (fun configPath _ ->
                        isPathPartOfAnyPaths configPath updatedPaths)

                { loadedConfigs with
                    PathsAdded = updatedPaths
                    LoadedConfigs = updatedConfigs }

        /// With a given list of paths, any paths loaded not in the list will be removed
        /// and any in the list but not loaded will be added.
        let updatePaths tryLoadConfig loadedConfigs paths =
            let existingPaths = Set.ofList loadedConfigs.PathsAdded
            let newPaths = Set.ofList paths

            let pathsToAdd = Set.difference newPaths existingPaths |> List.ofSeq

            let pathsToRemove = Set.difference existingPaths newPaths |> List.ofSeq

            let loadedConfigs = pathsToAdd |> List.fold (addPath tryLoadConfig) loadedConfigs
            let loadedConfigs = pathsToRemove |> List.fold removePath loadedConfigs

            loadedConfigs

        let rec private transpose matrix =
            match matrix with
            | (col::cols)::rows ->
                let first = List.map (function [] -> None | h::_ -> Some h) matrix
                let rest = transpose (List.map (function [] -> [] | _::t -> t) matrix)
                first :: rest
            | _ -> []

        /// Attempts to get a path that is common to all paths
        /// that have been added to a node.
        /// If a given preferred path is found to be a common path then
        /// that path will always be returned, useful if you want to prefer
        /// the solution directory for example.
        let commonPath loadedConfigs preferredPath =
            let commonPath =
                transpose loadedConfigs.PathsAdded
                |> Seq.takeWhile (function
                    | (first::_) as segments -> List.forall ((=) first) segments
                    | [] -> false)
                |> Seq.toList
                |> List.choose List.head

            if List.isEmpty commonPath then None
            else if listStartsWith (commonPath, preferredPath) then Some preferredPath
            else Some commonPath

        /// Tries to reload the configuration for all paths.
        /// Call when the user has edited a configuration file on disk.
        let refresh tryLoadConfig loadedConfigs =
            { loadedConfigs with
                  LoadedConfigs =
                    loadedConfigs.LoadedConfigs
                    |> Map.map (fun configPath _ -> tryLoadConfig configPath)
                  GlobalConfigs =
                    loadedConfigs.GlobalConfigs
                    |> List.map (fun x -> { x with Configuration = tryLoadConfig x.Path }) }

        /// Gets the configuration file located at a given path.
        /// The configuration file returned may be incomplete as it
        /// will not have overrided any previous configuration files.
        let getPartialConfig loadedConfigs path =
            let config = Map.tryFind path loadedConfigs.LoadedConfigs

            match config with
            | Some(Some(config)) -> Some(config)
            | Some(None) | None ->
                List.tryFind (fun { Path = x } -> x = path) loadedConfigs.GlobalConfigs
                |> Option.bind (fun x -> x.Configuration)

        let private tryOverrideConfig configToOverride config =
            match (configToOverride, config) with
            | Some(configToOverride), Some(config) ->
                Some(overrideConfiguration configToOverride config)
            | Some(x), None
            | None, Some(x) -> Some(x)
            | None, None -> None

        /// Gets the complete configuration file located at a given path.
        /// "complete" configuration means that it has overridden any previous
        /// configuration files.
        let getConfig loadedConfigs path =
            let (globalConfigs, pathWasAGlobalConfig) =
                let rec takeUntilPathMatch built = function
                | config::_ when config.Path = path -> (config::built, true)
                | config::rest -> takeUntilPathMatch (config::built) rest
                | [] -> (built, false)

                takeUntilPathMatch [] loadedConfigs.GlobalConfigs
                |> fun (configs, matchFound) -> (List.rev configs, matchFound)

            let globalConfig =
                globalConfigs
                |> List.map (fun x -> x.Configuration)
                |> List.fold tryOverrideConfig (Some defaultConfiguration)

            if pathWasAGlobalConfig then globalConfig
            else
                getAllPaths path
                |> List.fold (fun config path ->
                    match loadedConfigs.LoadedConfigs.TryFind path with
                    | Some(loadedConfig) -> tryOverrideConfig config loadedConfig
                    | None -> config) globalConfig

        /// Updates a configuration file at a given path.
        let updateConfig loadedConfigs path config =
            { loadedConfigs with
                  LoadedConfigs =
                    loadedConfigs.LoadedConfigs
                    |> Map.map (fun key value -> if key = path then config else value)
                  GlobalConfigs =
                    loadedConfigs.GlobalConfigs
                    |> List.map (fun globalConfig ->
                        if globalConfig.Path = path then { globalConfig with Configuration = config }
                        else globalConfig) }

        /// Tries to normalise paths to a format that can be used as a path in <see cref="LoadedConfigs" />.
        let normalisePath (path:string) =
            Path.GetFullPath path
            |> fun x -> x.Split([|Path.DirectorySeparatorChar|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList