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
                        <BooleanProperty Name="Enabled">True</BooleanProperty>
                        <BooleanProperty Name="Enabled">True</BooleanProperty>
                        <StringProperty Name="Enabled">True</StringProperty>
                    </RuleSettings>
                </Rule>
                <Rule Name="InterfaceNamesMustBeginWithI">
                    <RuleSettings>
                        <BooleanProperty Name="Enabled">True</BooleanProperty>
                    </RuleSettings>
                </Rule>
            </Rules>
            <AnalyzerSettings />
        </Analyzer>
        <Analyzer AnalyzerId="FSharpLint.NamingRules">
            <Rules>
                <Rule Name="InterfaceNamesMustBeginWithI">
                    <RuleSettings>
                        <BooleanProperty Name="Enabled">True</BooleanProperty>
                    </RuleSettings>
                </Rule>
            </Rules>
            <AnalyzerSettings />
        </Analyzer>
    </Analyzers>
</FSharpLintSettings>""">

    type RuleSetting =
        | Enabled of bool

    let areSameRule lhs rhs =
        match lhs, rhs with
            | Enabled(_), Enabled(_) -> true

    type Rule = 
        {
            Settings: RuleSetting list
        }

        member this.IsEnabled() =
            let isSettingEnabled = function
                | Enabled(true) -> true
                | _ -> false

            this.Settings |> List.find isSettingEnabled
        
    let private parseRule (rule:Config.DomainTypes.Rule) =
        {
            Settings =
                [
                    for setting in rule.RuleSettings.GetBooleanProperties() do
                        match setting.Name with
                            | "Enabled" -> yield Enabled(setting.Value)
                            | _ -> ()
                ]
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
                yield (analyser.AnalyzerId, getRules analyser) 
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

    let private overrideRule oldRule newRule =
        {
            Settings =
                [
                    for oldRule in oldRule.Settings do
                        let rule = newRule.Settings |> List.tryFind (areSameRule oldRule)

                        match rule with
                            | Some(rule) -> yield rule
                            | None -> yield oldRule
                ]
        }

    let private overrideRules oldRules newRules =
        overwriteMap oldRules newRules overrideRule

    let overrideConfiguration configToOverride file =
        let newAnalysers = configuration file

        overwriteMap configToOverride newAnalysers overrideRules
        
    let loadDefaultConfiguration () =
        System.IO.File.ReadAllText("DefaultConfiguration.FSharpLint") |> configuration