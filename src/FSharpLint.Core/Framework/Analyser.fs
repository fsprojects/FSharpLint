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

    [<NoComparison; NoEquality>]
    type AnalysisArgs = 
        { Context: Ast.VisitorInfo
          CheckResults: FSharpCheckFileResults option
          SyntaxArray: AbstractSyntaxArray.Node []
          SkipArray: AbstractSyntaxArray.Skip [] }

    type Rule(analyserName, name, code, ruleConfig) =
        member __.Name = name
        member __.Code = code

        member __.Enabled = true

        member __.MessageFormat([<ParamArray>] args: obj []) = 
            String.Format(Resources.GetString name, args)

        member __.IsSuppressed analysisArgs i =
            AbstractSyntaxArray.getSuppressMessageAttributes analysisArgs.SyntaxArray analysisArgs.SkipArray i 
            |> AbstractSyntaxArray.isRuleSuppressed analyserName name

        member this.NotSuppressed analysisArgs i = this.IsSuppressed analysisArgs i |> not
    
    [<AbstractClass>]
    type Analyser(name, code, config) =
        member __.Name = name
        member __.Code = code
        member __.Enabled = true

        member __.Rule(ruleName, code, ruleConfig) = Rule(name, ruleName, code, ruleConfig)

        abstract member Analyse: AnalysisArgs -> unit