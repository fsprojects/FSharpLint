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

module Configuration =

    type Config = XmlProvider<"../FSharpLint.Framework/DefaultConfiguration.FSharpLint", Global = true>

    exception ConfigurationException of string

    type Property =
        | Enabled of bool
        | Lines of int
        | Depth of int
        | MaxItems of int
        | Length of int
        | Hints of string list

    type Rule = 
        {
            Settings: Map<string, Property>
        }

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
            | "Hints" when property.String.IsSome ->
                Some(Hints(property.String.Value.Split([|'\n'|]) |> Seq.map (fun x -> x.Trim()) |> Seq.toList))
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

    let loadFileFromFileSystem path = System.IO.File.ReadAllText(path)

    let overrideConfiguration configToOverride file =
        let newAnalysers = loadFileFromFileSystem file |> configuration

        overwriteMap configToOverride newAnalysers overrideAnalysers
        
    let loadDefaultConfiguration () =
        let assembly = System.Reflection.Assembly.GetExecutingAssembly()
        let resourceName = "DefaultConfiguration.FSharpLint"

        use stream = assembly.GetManifestResourceStream(resourceName)
        use reader = new System.IO.StreamReader(stream)

        reader.ReadToEnd() |> configuration

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

    let isRuleEnabled (config:Map<string,Analyser>) analyserName ruleName =
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