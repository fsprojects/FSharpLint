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

module Analyser =

    open System
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open AbstractSyntaxArray

    [<NoComparison; NoEquality>]
    type AnalysisArgs = 
        { Context: Ast.VisitorInfo
          CheckResults: FSharpCheckFileResults option
          SyntaxArray: AbstractSyntaxArray.Node []
          SkipArray: AbstractSyntaxArray.Skip [] }

    let getBreadcrumbs maxBreadcrumbs analysisArgs i =
        let syntaxArray, skipArray = analysisArgs.SyntaxArray, analysisArgs.SkipArray
        let rec getBreadcrumbs breadcrumbs i =
            if i = 0 then
                let node = syntaxArray.[i].Actual
                node::breadcrumbs
            else if i < skipArray.Length && (List.length breadcrumbs) < maxBreadcrumbs then
                let node = syntaxArray.[i].Actual
                let parenti = skipArray.[i].ParentIndex
                getBreadcrumbs (node::breadcrumbs) parenti
            else
                breadcrumbs

        if i = 0 then [] 
        else getBreadcrumbs [] (skipArray.[i].ParentIndex) |> List.rev

    let getSuppressMessageAttributes (syntaxArray: Node []) (skipArray: Skip []) i =
        let rec getSuppressMessageAttributes breadcrumbs i =
            if i = 0 then
                let node = Ast.getSuppressMessageAttributes syntaxArray.[i].Actual
                if List.isEmpty node then breadcrumbs
                else node::breadcrumbs
            else if i < skipArray.Length then
                let node = Ast.getSuppressMessageAttributes syntaxArray.[i].Actual
                let parenti = skipArray.[i].ParentIndex
                if List.isEmpty node then
                    getSuppressMessageAttributes breadcrumbs parenti
                else
                    getSuppressMessageAttributes (node::breadcrumbs) parenti
            else
                breadcrumbs

        getSuppressMessageAttributes [] i

    [<Literal>]
    let SuppressRuleWildcard = "*"

    let isRuleSuppressed analyserName ruleName suppressedRuleAttributes =
        let isSuppressed (l:Ast.SuppressedMessage, _) = 
            l.Category = analyserName && (l.Rule = SuppressRuleWildcard || l.Rule = ruleName)

        suppressedRuleAttributes
        |> List.exists (List.exists isSuppressed)

    type Rule(analyserName, name, code, ruleConfig) =
        member __.Name = name
        member __.Code = code

        member __.Enabled = true

        member __.MessageFormat([<ParamArray>] args: obj []) = 
            String.Format(Resources.GetString name, args)

        member __.IsSuppressed analysisArgs i =
            getSuppressMessageAttributes analysisArgs.SyntaxArray analysisArgs.SkipArray i 
            |> isRuleSuppressed analyserName name

        member this.NotSuppressed analysisArgs i = this.IsSuppressed analysisArgs i |> not
    
    [<AbstractClass>]
    type Analyser(name:string, code, config) =
        member __.Name = name
        member __.Code = code
        member __.Enabled = true

        member __.Rule(ruleName, code, ruleConfig) = Rule(name, ruleName, code, ruleConfig)

        abstract member Analyse: AnalysisArgs -> unit