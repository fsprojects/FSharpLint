/// Old XML style config, to support conversion from old XML config to new JSON config.
module FSharpLint.Application.XmlConfiguration

open FSharpLint.Rules
open System.IO
open ConfigurationManager
open FSharpLint.Framework.Rules
open FSharpLint.Rules.TypedItemSpacing

/// Loads configuration files from xml into an object.
/// When a configuration file has already been loaded, loading another one overwrites the existing configuration.
/// The overwrite works by only changing existing properties with properties from the new file,
/// so properties in the original configuration file not in the new configuration file will remain.
module Configuration =

    open System.Xml.Linq
    open System.Reflection

    [<Literal>]
    let private SettingsFileName = "Settings.FSharpLint"

    [<Literal>]
    let private Namespace = @"https://github.com/fsprojects/FSharpLint/blob/master/ConfigurationSchema.xsd"

    let private getName name =
        XName.Get(name, Namespace)

    type Update =
        | Add
        | Overwrite

        static member From(el:XElement) =
            let updateAttribute = el.Attributes() |> Seq.tryFind (fun x -> x.Name.LocalName = "Update")

            match updateAttribute with
            | Some(attribute) when attribute.Value.ToUpperInvariant() = "ADD" -> Add
            | Some(_) | None -> Overwrite

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

    type XmlNaming =
        | PascalCase = 0
        | CamelCase = 1

    type XmlNamingUnderscores =
        | AllowPrefix = 0
        | AllowAny = 1
        | None = 2

    type XmlTypedItemStyle =
        | NoSpaces = 0
        | SpaceAfter = 1
        | SpacesAround = 2

    type Hints = { Hints: string list }

    type Setting =
        | Enabled of bool
        | Lines of int
        | Depth of int
        | MaxItems of int
        | Length of int
        | Hints of Hints
        | OneSpaceAllowedAfterOperator of bool
        | NumberOfSpacesAllowed of int
        | NumberOfIndentationSpaces of int
        | IgnoreBlankLines of bool
        | Access of Access
        | Naming of XmlNaming
        | Prefix of string option
        | Suffix of string option
        | Underscores of XmlNamingUnderscores
        | TypedItemStyle of XmlTypedItemStyle

    let private parseLines (content:string) =
        content.Split('\n')
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
        |> Seq.toList

    module IgnoreFiles =

        open System
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

        [<NoComparison>]
        type IgnoreFilesConfig =
            { Content: string list}

        let private parseIgnoreFiles (ignoreFiles:XElement) =
            { Content = ignoreFiles.Value.Trim() |> parseLines }

        let getIgnorePathsFromConfig (configRoot:XElement) =
            match configRoot.ElementByLocalName("IgnoreFiles") with
            | Some(ignoreFilesElement) -> parseIgnoreFiles ignoreFilesElement |> Some
            | None -> None

    type Rule =  { Settings: Map<string, Setting> }

    /// An analyser groups together related rules in the configuration file.
    type Analyser =
        { Settings: Map<string, Setting>
          Rules: Map<string, Rule> }

    [<NoComparison>]
    type XmlConfiguration =
        { IgnoreFiles: IgnoreFiles.IgnoreFilesConfig option
          Analysers: Map<string, Analyser> }

    let private fromEnum name value =
        let (valid, ret) = System.Enum.TryParse(value)
        if not valid then sprintf "Found unknown XmlDocumentation %s value %s" name value |> ConfigurationException |> raise
        ret

    let private parseHints (el:XElement) =
        let hintsText = el.Value
        let hints =
            parseLines hintsText
            |> List.filter (System.String.IsNullOrWhiteSpace >> not)

        { Hints = hints }

    let private emptyStringAsNone str =
        if System.String.IsNullOrWhiteSpace str then None else Some str

    let private parseSetting (setting:XElement) =
        match setting.Name.LocalName with
        | "Enabled" -> Enabled(setting.Value |> bool.Parse)
        | "Lines" -> Lines(setting.Value |> int)
        | "Depth" -> Depth(setting.Value |> int)
        | "Length" -> Length(setting.Value |> int)
        | "MaxItems" -> MaxItems(setting.Value |> int)
        | "Hints" -> Hints(parseHints setting)
        | "OneSpaceAllowedAfterOperator" -> OneSpaceAllowedAfterOperator(setting.Value |> bool.Parse)
        | "NumberOfSpacesAllowed" -> NumberOfSpacesAllowed(setting.Value |> int)
        | "NumberOfIndentationSpaces" -> NumberOfIndentationSpaces(setting.Value |> int)
        | "TypedItemStyle" -> TypedItemStyle(setting.Value |> fromEnum "TypedItemStyle")
        | "IgnoreBlankLines" -> IgnoreBlankLines(setting.Value |> bool.Parse)
        | "Access" -> Access(setting.Value |> fromEnum "Access")
        | "Naming" -> Naming(setting.Value |> fromEnum "Naming")
        | "Prefix" -> Prefix(emptyStringAsNone setting.Value)
        | "Suffix" -> Suffix(emptyStringAsNone setting.Value)
        | "Underscores" -> Underscores(setting.Value |> fromEnum "NamingUnderscores")
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

    /// Parse a configuration file.
    let configuration (file:string) =
        use configReader = new System.IO.StringReader(file)
        let config = XDocument.Load(configReader).Root

        { IgnoreFiles = IgnoreFiles.getIgnorePathsFromConfig config
          Analysers =
            match config.ElementByLocalName("Analysers") with
            | Some(analysers) -> analysers.Elements() |> Seq.map parseAnalyser |> Map.ofSeq
            | None -> Map.empty }

     /// Load a FSharpLint configuration file from the contents (string) of the file.
    let loadConfigurationFile configurationFileText =
        configuration configurationFileText

    let overwriteMap (oldMap:Map<'a,'b>) (newMap:Map<'a,'b>) overwriteValue =
        [ for keyValuePair in oldMap do
            match Map.tryFind keyValuePair.Key newMap with
            | Some(value) -> yield (keyValuePair.Key, overwriteValue keyValuePair.Value value)
            | None -> yield (keyValuePair.Key, keyValuePair.Value) ] |> Map.ofList

    let private overrideRuleSettings (oldSetting:Setting) (newSetting:Setting) =
        match oldSetting, newSetting with
        | Hints(oldHints), Hints(newHints) ->
            Hints({ Hints = List.append oldHints.Hints newHints.Hints })
        | _ -> newSetting

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
        { IgnoreFiles =
                match configToOverrideWith.IgnoreFiles with
                | Some({ Content = ignoreFiles } as newIgnore) ->
                    let combinedFiles =
                        match configToOverride.IgnoreFiles with
                        | Some (previousIgnore) ->
                            newIgnore.Content @ previousIgnore.Content
                        | None -> newIgnore.Content

                    { IgnoreFiles.Content = combinedFiles } |> Some
                | None ->
                    configToOverride.IgnoreFiles
          Analysers = overwriteMap configToOverride.Analysers configToOverrideWith.Analysers overrideAnalysers }

    /// Overrides a given FSharpLint configuration file with another.
    let overrideConfigurationFile configurationToOverride configurationToOverrideWith =
        overrideConfiguration configurationToOverride configurationToOverrideWith

    /// A default configuration specifying every analyser and rule is included as a resource file in the framework.
    /// This function loads and returns this default configuration.
    let defaultConfiguration =
        let assembly = typeof<Configuration>.GetTypeInfo().Assembly
        let resourceName = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                         |> Seq.find (fun n -> n.EndsWith("DefaultConfiguration.FSharpLint", System.StringComparison.Ordinal))
        use stream = assembly.GetManifestResourceStream(resourceName)
        match stream with
        | null -> failwithf "Resource '%s' not found in assembly '%s'" resourceName (assembly.FullName)
        | stream ->
            use reader = new System.IO.StreamReader(stream)

            reader.ReadToEnd() |> configuration

    /// Overrides the default FSharpLint configuration.
    /// The default FSharpLint configuration contains all required elements, so
    /// by overriding it any missing required elements will be added to the returned configuration.
    /// If you're loading your own configuration you should make sure that it overrides the default
    /// configuration/overrides a configuration that has overriden the default configuration.
    let overrideDefaultConfiguration configurationToOverrideDefault =
        overrideConfiguration defaultConfiguration configurationToOverrideDefault

    /// Gets all the parent directories of a given path - includes the original path directory too.
    let private getParentDirectories path =
        let rec getParentDirectories parentDirectories (directoryInfo:DirectoryInfo) =
            match directoryInfo with
            | null -> parentDirectories
            | _ -> getParentDirectories (directoryInfo::parentDirectories) directoryInfo.Parent

        DirectoryInfo path |> getParentDirectories []

    /// Overrides the default config with user defined config files.
    /// The configs can be in any directory between the root directory and the projects directory.
    /// The closer they are to the project directory the higher precedence they have.
    /// e.g. if the project directory is C:\User\Matt\Project then a config file found in
    /// C:\User\ will be loaded before and overridden by a config file found in C:\User\Matt\.
    let tryLoadUserConfigFiles (projectFilePath: string) =
        let mutable foundConfig = false
        let projectFileDirectory = Path.GetDirectoryName projectFilePath
        let subdirectories = getParentDirectories projectFileDirectory |> List.map (fun x -> x.FullName)

        let rec loadAllConfigs configToOveride = function
            | path::paths ->
                let filename = Path.Combine(path, SettingsFileName)

                if File.Exists(filename) then
                    try
                        let newConfig =
                            File.ReadAllText filename
                            |> configuration
                            |> (overrideConfiguration configToOveride)

                        foundConfig <- true
                        loadAllConfigs newConfig paths
                    with
                        | _ -> None
                else
                    loadAllConfigs configToOveride paths
            | [] ->
                Some configToOveride

        let config = loadAllConfigs defaultConfiguration subdirectories

        if foundConfig then config else None

/// Tries to load a config from disk.
/// If it fails to load the config any exception will be swallowed and `None` returned.
/// If the file does not exist `None` will be returned.
let private tryLoadConfig filePath =
    if File.Exists(filePath) then
        try File.ReadAllText filePath |> Configuration.configuration |> Some
        with _ -> None
    else
        None

let convertRuleWithConfig (config:Configuration.XmlConfiguration) (analyserName:string) (ruleName:string option) buildConfig =
    let isRuleEnabled (analyserEnabled:Configuration.Setting option) (ruleEnabled:Configuration.Setting option) =
        match (analyserEnabled, ruleEnabled) with
        | (Some (Configuration.Setting.Enabled true), Some (Configuration.Setting.Enabled rule))
        | (None, Some (Configuration.Setting.Enabled rule)) -> rule
        | (None, None) -> true
        | _ -> false

    match Map.tryFind analyserName config.Analysers with
    | Some analyser ->
        let analyserEnabled = Map.tryFind "Enabled" analyser.Settings
        match ruleName with
        | Some ruleName ->
            match Map.tryFind ruleName analyser.Rules with
            | Some rule ->
                let ruleEnabled = Map.tryFind "Enabled" rule.Settings
                Some { RuleConfig.enabled = isRuleEnabled analyserEnabled ruleEnabled; config = buildConfig rule.Settings }
            | None ->
                None
        | None ->
            Some { RuleConfig.enabled = isRuleEnabled analyserEnabled (Some (Configuration.Setting.Enabled true)); config = buildConfig analyser.Settings }
    | None ->
        None

let convertRuleNoConfig (config:Configuration.XmlConfiguration) (analyserName:string) (ruleName:string option) =
    convertRuleWithConfig config analyserName ruleName (fun _ -> None)

let convertHints (config:Configuration.XmlConfiguration) =
    let analyser = Map.tryFind "Hints" config.Analysers

    match analyser with
    | Some analyser ->
        match Map.tryFind "Hints" analyser.Settings with
        | Some(Configuration.Hints(hints)) ->
            Some { HintConfig.add = hints.Hints |> Array.ofList |> Some; ignore = None }
        | _ ->
            None
    | None ->
        None

let convertIgnoreFiles (config:Configuration.XmlConfiguration) =
    config.IgnoreFiles
    |> Option.map (fun files -> files.Content |> Array.ofList)

let convertFormatting (config:Configuration.XmlConfiguration) =
    let formattingAnalyser = "Formatting"
    let convertTypedItemSpacing (config:Configuration.XmlConfiguration) =
        let buildConfig settings =
            match Map.tryFind "TypedItemStyle" settings with
            | Some (Configuration.Setting.TypedItemStyle style) ->
                let convertedStyle =
                    match style with
                    | Configuration.XmlTypedItemStyle.NoSpaces -> TypedItemStyle.NoSpaces
                    | Configuration.XmlTypedItemStyle.SpaceAfter -> TypedItemStyle.SpaceAfter
                    | Configuration.XmlTypedItemStyle.SpacesAround -> TypedItemStyle.SpacesAround
                    | _ -> TypedItemStyle.SpacesAround

                Some { TypedItemSpacing.Config.typedItemStyle = convertedStyle }
            | _ -> None

        convertRuleWithConfig config formattingAnalyser (Some "TypedItemSpacing") buildConfig

    let convertTupleFormatting (config:Configuration.XmlConfiguration) =
        { TupleFormattingConfig.tupleIndentation = convertRuleNoConfig config formattingAnalyser (Some "TupleIndentation")
          tupleCommaSpacing = convertRuleNoConfig config formattingAnalyser (Some "TupleCommaSpacing")
          tupleParentheses = convertRuleNoConfig config formattingAnalyser (Some "TupleParentheses") } |> Some

    let convertPatternMatchFormatting (config:Configuration.XmlConfiguration) =
        { PatternMatchFormattingConfig.patternMatchClauseIndentation = convertRuleNoConfig config formattingAnalyser (Some "PatternMatchClauseIndentation")
          patternMatchClausesOnNewLine = convertRuleNoConfig config formattingAnalyser (Some "PatternMatchClausesOnNewLine")
          patternMatchOrClausesOnNewLine = convertRuleNoConfig config formattingAnalyser (Some "PatternMatchOrClausesOnNewLine")
          patternMatchExpressionIndentation =  convertRuleNoConfig config formattingAnalyser (Some "PatternMatchExpressionIndentation") } |> Some

    { FormattingConfig.typedItemSpacing = convertTypedItemSpacing config
      typePrefixing = convertRuleNoConfig config formattingAnalyser (Some "TypePrefixing")
      unionDefinitionIndentation = convertRuleNoConfig config formattingAnalyser (Some "UnionDefinitionIndentation")
      moduleDeclSpacing = convertRuleNoConfig config formattingAnalyser (Some "ModuleDeclSpacing")
      classMemberSpacing = convertRuleNoConfig config formattingAnalyser (Some "ClassMemberSpacing")
      tupleFormatting = convertTupleFormatting config
      patternMatchFormatting = convertPatternMatchFormatting config } |> Some

let convertConventions (config:Configuration.XmlConfiguration) =
    let convertNestedStatements (config:Configuration.XmlConfiguration) =
        let buildConfig settings =
            match Map.tryFind "Depth" settings with
            | Some (Configuration.Setting.Depth depth) ->
                  Some { NestedStatements.Config.depth = depth }
            | _ -> None

        convertRuleWithConfig config "NestedStatements" None buildConfig

    let convertRaiseWithTooManyArgs (config:Configuration.XmlConfiguration) =
        let raiseWithTooManyArgsAnalyser = "RaiseWithTooManyArguments"
        { RaiseWithTooManyArgsConfig.raiseWithSingleArgument = convertRuleNoConfig config raiseWithTooManyArgsAnalyser (Some "RaiseWithSingleArgument")
          nullArgWithSingleArgument = convertRuleNoConfig config raiseWithTooManyArgsAnalyser (Some "NullArgWithSingleArgument")
          invalidOpWithSingleArgument = convertRuleNoConfig config raiseWithTooManyArgsAnalyser (Some "InvalidOpWithSingleArgument")
          invalidArgWithTwoArguments = convertRuleNoConfig config raiseWithTooManyArgsAnalyser (Some "InvalidArgWithTwoArguments")
          failwithfWithArgumentsMatchingFormatString = convertRuleNoConfig config raiseWithTooManyArgsAnalyser (Some "FailwithfWithArgumentsMatchingFormatString") } |> Some

    let convertSourceLength (config:Configuration.XmlConfiguration) =
        let convertSourceLengthConfig (ruleName:string) =
            let buildConfig settings =
                match Map.tryFind "Lines" settings with
                | Some (Configuration.Setting.Lines lines) ->
                      Some { Helper.SourceLength.Config.maxLines = lines }
                | _ -> None
            convertRuleWithConfig config "SourceLength" (Some ruleName) buildConfig

        { SourceLengthConfig.maxLinesInLambdaFunction = convertSourceLengthConfig "MaxLinesInLambdaFunction"
          maxLinesInMatchLambdaFunction = convertSourceLengthConfig "MaxLinesInMatchLambdaFunction"
          maxLinesInValue = convertSourceLengthConfig "MaxLinesInValue"
          maxLinesInFunction = convertSourceLengthConfig "MaxLinesInFunction"
          maxLinesInMember = convertSourceLengthConfig "MaxLinesInMember"
          maxLinesInConstructor = convertSourceLengthConfig "MaxLinesInConstructor"
          maxLinesInProperty = convertSourceLengthConfig "MaxLinesInProperty"
          maxLinesInModule = convertSourceLengthConfig "MaxLinesInModule"
          maxLinesInRecord = convertSourceLengthConfig "MaxLinesInRecord"
          maxLinesInEnum = convertSourceLengthConfig "MaxLinesInEnum"
          maxLinesInUnion = convertSourceLengthConfig "MaxLinesInUnion"
          maxLinesInClass = convertSourceLengthConfig "MaxLinesInClass" } |> Some

    let convertNaming (config:Configuration.XmlConfiguration) =
        let convertNamingConfig (ruleName:string) =
            let buildConfig settings =
                let namingCase =
                    match Map.tryFind "Naming" settings with
                    | Some (Configuration.Setting.Naming naming) ->
                        match naming with
                        | Configuration.XmlNaming.CamelCase -> Some NamingCase.CamelCase
                        | Configuration.XmlNaming.PascalCase -> Some NamingCase.PascalCase
                        | _ -> None
                    | _ -> None

                let underscores =
                    match Map.tryFind "Underscores" settings with
                    | Some (Configuration.Setting.Underscores underscores) ->
                        match underscores with
                        | Configuration.XmlNamingUnderscores.AllowPrefix -> Some NamingUnderscores.AllowPrefix
                        | Configuration.XmlNamingUnderscores.AllowAny -> Some NamingUnderscores.AllowAny
                        | Configuration.XmlNamingUnderscores.None -> Some NamingUnderscores.None
                        | _ -> None
                    | _ -> None

                let prefix =
                    match Map.tryFind "Prefix" settings with
                    | Some (Configuration.Setting.Prefix prefix) -> prefix
                    | _ -> None

                let suffix =
                    match Map.tryFind "Suffix" settings with
                    | Some (Configuration.Setting.Suffix suffix) -> suffix
                    | _ -> None

                { NamingConfig.naming = namingCase
                  underscores = underscores
                  prefix = prefix
                  suffix = suffix } |> Some

            convertRuleWithConfig config "NameConventions" (Some ruleName) buildConfig

        { NamesConfig.interfaceNames = convertNamingConfig "InterfaceNames"
          exceptionNames = convertNamingConfig "ExceptionNames"
          typeNames = convertNamingConfig "TypeNames"
          recordFieldNames = convertNamingConfig "RecordFieldNames"
          enumCasesNames = convertNamingConfig "EnumCasesNames"
          unionCasesNames = convertNamingConfig "UnionCasesNames"
          moduleNames = convertNamingConfig "ModuleNames"
          literalNames = convertNamingConfig "LiteralNames"
          namespaceNames = convertNamingConfig "NamespaceNames"
          memberNames = convertNamingConfig "MemberNames"
          parameterNames = convertNamingConfig "ParameterNames"
          measureTypeNames = convertNamingConfig "MeasureTypeNames"
          activePatternNames = convertNamingConfig "ActivePatternNames"
          publicValuesNames = convertNamingConfig "PublicValuesNames"
          nonPublicValuesNames = convertNamingConfig "NonPublicValuesNames" } |> Some

    let convertNumberOfItems (config:Configuration.XmlConfiguration) =
        let convertMaxItemsConfig (ruleName:string) =
            let buildConfig settings =
                match Map.tryFind "MaxItems" settings with
                | Some (Configuration.Setting.MaxItems maxItems) ->
                      Some { Helper.NumberOfItems.Config.maxItems = maxItems }
                | _ -> None
            convertRuleWithConfig config "NumberOfItems" (Some ruleName) buildConfig

        { NumberOfItemsConfig.maxNumberOfItemsInTuple = convertMaxItemsConfig "MaxNumberOfItemsInTuple"
          maxNumberOfFunctionParameters = convertMaxItemsConfig "MaxNumberOfFunctionParameters"
          maxNumberOfMembers = convertMaxItemsConfig "MaxNumberOfMembers"
          maxNumberOfBooleanOperatorsInCondition = convertMaxItemsConfig "MaxNumberOfBooleanOperatorsInCondition" } |> Some

    let convertBinding (config:Configuration.XmlConfiguration) =
        let bindingAnalyser = "Binding"
        { BindingConfig.favourIgnoreOverLetWild = convertRuleNoConfig config bindingAnalyser (Some "FavourIgnoreOverLetWild")
          wildcardNamedWithAsPattern = convertRuleNoConfig config bindingAnalyser (Some "WildcardNamedWithAsPattern")
          uselessBinding = convertRuleNoConfig config bindingAnalyser (Some "UselessBinding")
          tupleOfWildcards = convertRuleNoConfig config bindingAnalyser (Some "TupleOfWildcards") } |> Some

    { ConventionsConfig.recursiveAsyncFunction = convertRuleNoConfig config "Conventions" (Some "RecursiveAsyncFunction")
      redundantNewKeyword = convertRuleNoConfig config "RedundantNewKeyword" None
      nestedStatements = convertNestedStatements config
      reimplementsFunction = convertRuleNoConfig config "FunctionReimplementation" (Some "ReimplementsFunction")
      canBeReplacedWithComposition = convertRuleNoConfig config "FunctionReimplementation" (Some "CanBeReplacedWithComposition")
      raiseWithTooManyArgs = convertRaiseWithTooManyArgs config
      sourceLength = convertSourceLength config
      naming = convertNaming config
      numberOfItems = convertNumberOfItems config
      binding = convertBinding config } |> Some

let convertTypography (config:Configuration.XmlConfiguration) =
    let typographyAnalyser = "Typography"
    let convertIndentation (config:Configuration.XmlConfiguration) =
        let buildConfig settings =
            match Map.tryFind "NumberOfIndentationSpaces" settings with
            | Some (Configuration.Setting.NumberOfIndentationSpaces spaces) ->
                  Some { Indentation.Config.numberOfIndentationSpaces = spaces }
            | _ -> None

        convertRuleWithConfig config typographyAnalyser (Some "Indentation") buildConfig

    let convertMaxCharactersOnLine (config:Configuration.XmlConfiguration) =
        let buildConfig settings =
            match Map.tryFind "Length" settings with
            | Some (Configuration.Setting.Length length) ->
                  Some { MaxCharactersOnLine.Config.maxCharactersOnLine = length }
            | _ -> None

        convertRuleWithConfig config typographyAnalyser (Some "MaxCharactersOnLine") buildConfig

    let convertMaxLinesInFile (config:Configuration.XmlConfiguration) =
        let buildConfig settings =
            match Map.tryFind "Lines" settings with
            | Some (Configuration.Setting.Lines lines) ->
                  Some { MaxLinesInFile.Config.maxLinesInFile = lines }
            | _ -> None

        convertRuleWithConfig config typographyAnalyser (Some "MaxLinesInFile") buildConfig

    let convertTrailingWhitespaceOnLine (config:Configuration.XmlConfiguration) =
        let buildConfig settings =
            let numberOfSpacesAllowed =
                match Map.tryFind "NumberOfSpacesAllowed" settings with
                | Some (Configuration.Setting.NumberOfSpacesAllowed spaces) -> spaces
                | _ -> 0

            let oneSpaceAllowedAfterOperator =
                match Map.tryFind "OneSpaceAllowedAfterOperator" settings with
                | Some (Configuration.Setting.OneSpaceAllowedAfterOperator allowed) -> allowed
                | _ -> false

            let ignoreBlankLines =
                match Map.tryFind "IgnoreBlankLines" settings with
                | Some (Configuration.Setting.IgnoreBlankLines ignoring) -> ignoring
                | _ -> false

            { TrailingWhitespaceOnLine.Config.numberOfSpacesAllowed = numberOfSpacesAllowed
              TrailingWhitespaceOnLine.Config.oneSpaceAllowedAfterOperator = oneSpaceAllowedAfterOperator
              TrailingWhitespaceOnLine.Config.ignoreBlankLines = ignoreBlankLines } |> Some

        convertRuleWithConfig config typographyAnalyser (Some "TrailingWhitespaceOnLine") buildConfig

    {
        TypographyConfig.indentation = convertIndentation config
        maxCharactersOnLine = convertMaxCharactersOnLine config
        maxLinesInFile = convertMaxLinesInFile config
        trailingWhitespaceOnLine = convertTrailingWhitespaceOnLine config
        trailingNewLineInFile = convertRuleNoConfig config typographyAnalyser (Some "TrailingNewLineInFile")
        noTabCharacters = convertRuleNoConfig config typographyAnalyser (Some "NoTabCharacters")
    } |> Some

/// Tries to convert an old style XmlConfiguration to new JSON config.
let convertToConfig xmlConfig =
    { ignoreFiles = convertIgnoreFiles xmlConfig
      hints = convertHints xmlConfig
      formatting = convertFormatting xmlConfig
      conventions = convertConventions xmlConfig
      typography = convertTypography xmlConfig }

/// Tries to convert an old-format XML config file to the new JSON format.
let convertToJson (xmlFile:string) =
    let xmlConfig = Configuration.configuration xmlFile
    convertToConfig xmlConfig

/// Tries to load the FSharpLint XML configuration for a project given the path to the `.fsproj` file.
/// It picks up configurations in any directory between the root directory and the project's directory.
/// The closer they are to the project directory the higher precedence they have.
/// e.g. if the project directory is C:\User\Matt\Project then a config file found in
/// C:\User\ will be loaded before and overridden by a config file found in C:\User\Matt\.
/// If no XML configs are found, returns None.
let tryLoadConfigurationForProject projectFilePath =
    Configuration.tryLoadUserConfigFiles projectFilePath
