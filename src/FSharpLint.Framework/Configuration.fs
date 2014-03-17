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

    type Config = XmlProvider<"""<FSharpLintSettings>
    <Analyzers>
        <Analyzer AnalyzerId="FSharpLint.NamingRules">
            <Rules>
                <Rule Name="InterfaceNamesMustBeginWithI">
                    <RuleSettings>
                        <Property name="Enabled">True</Property>
                    </RuleSettings>
                </Rule>
                <Rule Name="InterfaceNamesMustBeginWithI">
                    <RuleSettings>
                        <Property name="Enabled">True</Property>
                    </RuleSettings>
                </Rule>
            </Rules>
            <AnalyzerSettings />
        </Analyzer>
        <Analyzer AnalyzerId="FSharpLint.NamingRules">
            <Rules>
                <Rule Name="InterfaceNamesMustBeginWithI">
                    <RuleSettings>
                        <Property name="Enabled">True</Property>
                        <Property name="Lines">5</Property>
                    </RuleSettings>
                </Rule>
            </Rules>
            <AnalyzerSettings />
        </Analyzer>
    </Analyzers>
</FSharpLintSettings>""">

    exception ConfigurationException of string

    type Property =
        | Enabled of bool
        | Lines of int

    type Rule = 
        {
            Settings: Map<string, Property>
        }

    type Analyser = { Rules: Map<string, Rule> }

    let private parseProperty (property:Config.DomainTypes.Property) =
        match property.Name with
            | "Enabled" when property.BooleanValue.IsSome -> 
                Some(Enabled(property.BooleanValue.Value))
            | "Lines" when property.NumberValue.IsSome -> 
                Some(Lines(property.NumberValue.Value))
            | _ -> 
                None
        
    let private parseRule (rule:Config.DomainTypes.Rule) =
        {
            Settings = 
                [ 
                    for property in rule.RuleSettings.GetProperties() do 
                        match parseProperty property with
                            | Some(propertyVal) -> 
                                yield (property.Name, propertyVal) 
                            | None -> ()
                ]
                    |> Map.ofList
        }

    let private getRules (analyser:Config.DomainTypes.Analyzer) =
        [ 
            for rule in analyser.Rules.GetRules() do 
                yield (rule.Name, parseRule rule) 
        ]
            |> Map.ofList

    /// Parse a configuration file.
    let configuration file = 
        let config = Config.Parse(file)
        [ 
            for analyser in config.Analyzers.GetAnalyzers() do 
                yield (analyser.AnalyzerId, { Rules = getRules analyser }) 
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

    let private overrideRule oldRule newRule =
        { Settings = overwriteMap oldRule.Settings newRule.Settings overrideRuleSettings }

    let private overrideRules oldRules newRules =
        { Rules = overwriteMap oldRules.Rules newRules.Rules overrideRule }

    let overrideConfiguration configToOverride file =
        let newAnalysers = configuration file

        overwriteMap configToOverride newAnalysers overrideRules
        
    let loadDefaultConfiguration () =
        System.IO.File.ReadAllText("DefaultConfiguration.FSharpLint") |> configuration