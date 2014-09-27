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

open FSharp.Data

/// Loads configuration files from xml into an object.
/// When a configuration file has already been loaded, loading another one overwrites the existing configuration.
/// The overwrite works by only changing existing properties with properties from the new file, 
/// so properties in the original configuration file not in the new configuration file will remain.
module Configuration =

    /// Represents a configuration XML file.
    type private Config = XmlProvider<"../FSharpLint.Framework/DefaultConfiguration.FSharpLint", Global = true>

    exception ConfigurationException of string

    type Property =
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

    type Rule = 
        {
            Settings: Map<string, Property>
        }

    /// An analyser groups together related rules in the configuration file.
    type Analyser = 
        { 
            Rules: Map<string, Rule> 
            Settings: Map<string, Property>
        }

    let private parseProperty (property:Config.Property) =
        match property.Name with
            | "Enabled" when property.Boolean.IsSome -> 
                Some(Enabled(property.Boolean.Value))
            | "Lines" when property.Number.IsSome -> 
                Some(Lines(property.Number.Value))
            | "Depth" when property.Number.IsSome -> 
                Some(Depth(property.Number.Value))
            | "Length" when property.Number.IsSome -> 
                Some(Length(property.Number.Value))
            | "MaxItems" when property.Number.IsSome -> 
                Some(MaxItems(property.Number.Value)) 
            | "MaxCyclomaticComplexity" when property.Number.IsSome -> 
                Some(MaxCyclomaticComplexity(property.Number.Value)) 
            | "IncludeMatchStatements" when property.Boolean.IsSome -> 
                Some(IncludeMatchStatements(property.Boolean.Value)) 
            | "Hints" when property.String.IsSome ->
                Some(Hints(property.String.Value.Split([|'\n'|]) |> Seq.map (fun x -> x.Trim()) |> Seq.toList))
            | "OneSpaceAllowedAfterOperator" when property.Boolean.IsSome -> 
                Some(OneSpaceAllowedAfterOperator(property.Boolean.Value))
            | "NumberOfSpacesAllowed" when property.Number.IsSome -> 
                Some(NumberOfSpacesAllowed(property.Number.Value))
            | "IgnoreBlankLines" when property.Boolean.IsSome -> 
                Some(IgnoreBlankLines(property.Boolean.Value))
            | _ -> 
                None

    let private parseSettings properties =
        [
            for property in properties do
                match parseProperty property with
                    | Some(propertyVal) -> 
                        yield (property.Name, propertyVal) 
                    | None -> ()
        ]

    let private parseRule (rule:Config.Rule) : Rule =
        {
            Settings = 
                [ 
                    let ruleSettings = rule.RuleSettings

                    yield! ruleSettings.Properties |> parseSettings
                ]
                    |> Map.ofList
        }

    let private getRules (analyser:Config.Analysers) =
        [ 
            for rule in analyser.Rules.Rules do
                yield (rule.Name, parseRule rule) 
        ]
            |> Map.ofList

    /// Parse a configuration file.
    let configuration file = 
        let config = Config.Parse(file)
        [ 
            for analyser in config.Analysers do 
                let settings = analyser.AnalyserSettings.Properties |> parseSettings |> Map.ofList

                yield (analyser.AnalyserId, { Rules = getRules analyser; Settings = settings }) 
        ]
            |> Map.ofList

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
    let overrideConfiguration configToOverride file =
        let newAnalysers = System.IO.File.ReadAllText(file) |> configuration

        overwriteMap configToOverride newAnalysers overrideAnalysers

    /// A default configuration specifying every analyser and rule is included as a resource file in the framework.
    /// This function loads and returns this default configuration.
    let loadDefaultConfiguration () =
        let assembly = System.Reflection.Assembly.GetExecutingAssembly()
        let resourceName = "DefaultConfiguration.FSharpLint"

        use stream = assembly.GetManifestResourceStream(resourceName)
        use reader = new System.IO.StreamReader(stream)

        reader.ReadToEnd() |> configuration

    /// Checks if a analyser in the configuration is enabled.
    /// Returns the analyser settings if the analyser was enabled; None otherwise.
    let isAnalyserEnabled (config:Map<string,Analyser>) analyserName =
        if not <| config.ContainsKey analyserName then
            raise <| ConfigurationException(sprintf "Expected %s analyser in config." analyserName)

        let analyserSettings = config.[analyserName].Settings

        if analyserSettings.ContainsKey "Enabled" then
            match analyserSettings.["Enabled"] with 
                | Enabled(enabled) when enabled -> Some(analyserSettings)
                | _ -> None
        else
            Some(analyserSettings)

    /// Checks if a rule in the configuration is enabled and the analyser it's within is also enabled.
    /// Returns the analyser settings and rule settings if the rule was enabled; None otherwise.
    let isRuleEnabled config analyserName ruleName =
        match isAnalyserEnabled config analyserName with
            | Some(analyserSettings) ->
                let rules = config.[analyserName].Rules

                if not <| rules.ContainsKey ruleName then 
                    let error = sprintf "Expected rule %s for %s analyser in config." ruleName analyserName
                    raise <| ConfigurationException(error)

                let ruleSettings = rules.[ruleName].Settings

                if ruleSettings.ContainsKey "Enabled" then
                    match ruleSettings.["Enabled"] with 
                        | Enabled(enabled) when enabled -> Some(analyserSettings, ruleSettings)
                        | _ -> None
                else
                    None
            | None -> None