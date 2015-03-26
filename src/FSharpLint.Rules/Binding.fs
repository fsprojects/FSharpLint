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

namespace FSharpLint.Rules

module Binding =
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadVisitors

    [<Literal>]
    let AnalyserName = "FSharpLint.Binding"

    let isRuleEnabled config ruleName =
        isRuleEnabled config AnalyserName ruleName |> Option.isSome

    type VisitorParameters =
        {
            VisitorInfo: VisitorInfo
            CheckFile: FSharpCheckFileResults
            AstNode: CurrentNode
        }
            
    /// Checks if any code uses 'let _ = ...' and suggests to use the ignore function.
    let checkForBindingToAWildcard visitorParameters pattern range =
        let isEnabled =
            "FavourIgnoreOverLetWild" |> isRuleEnabled visitorParameters.VisitorInfo.Config &&
            visitorParameters.AstNode.IsSuppressed(AnalyserName, "FavourIgnoreOverLetWild") |> not

        if isEnabled then
            let rec findWildAndIgnoreParens = function
                | SynPat.Paren(pattern, _) -> findWildAndIgnoreParens pattern
                | SynPat.Wild(_) -> true
                | _ -> false
                
            if findWildAndIgnoreParens pattern then
                visitorParameters.VisitorInfo.PostError 
                    range 
                    (FSharpLint.Framework.Resources.GetString("RulesFavourIgnoreOverLetWildError"))

    let checkForWildcardNamedWithAsPattern visitorParameters pattern =
        let isEnabled =
            "WildcardNamedWithAsPattern" |> isRuleEnabled visitorParameters.VisitorInfo.Config &&
            visitorParameters.AstNode.IsSuppressed(AnalyserName, "WildcardNamedWithAsPattern") |> not

        if isEnabled then
            match pattern with
                | SynPat.Named(SynPat.Wild(wildcardRange), _, _, _, range) when wildcardRange <> range ->
                    visitorParameters.VisitorInfo.PostError 
                        range 
                        (FSharpLint.Framework.Resources.GetString("RulesWildcardNamedWithAsPattern"))
                | _ -> ()

    let checkForUselessBinding visitorParameters pattern expr range =
        let isEnabled =
            "UselessBinding" |> isRuleEnabled visitorParameters.VisitorInfo.Config &&
            visitorParameters.AstNode.IsSuppressed(AnalyserName, "UselessBinding") |> not

        if isEnabled then
            let rec findBindingIdentifier = function
                | SynPat.Paren(pattern, _) -> findBindingIdentifier pattern
                | SynPat.Named(_, ident, _, _, _) -> Some(ident)
                | _ -> None

            let rec exprIdentMatchesBindingIdent (bindingIdent:Ident) = function
                | SynExpr.Paren(expr, _, _, _) -> 
                    exprIdentMatchesBindingIdent bindingIdent expr
                | SynExpr.Ident(ident) ->
                    let isSymbolMutable (ident:Ident) =
                        let symbol =
                            visitorParameters.CheckFile.GetSymbolUseAtLocation(ident.idRange.StartLine, ident.idRange.EndColumn, "", [ident.idText])
                                |> Async.RunSynchronously

                        let isMutable (symbol:FSharpSymbolUse) = 
                            (symbol.Symbol :?> FSharpMemberOrFunctionOrValue).IsMutable

                        symbol |> Option.exists isMutable

                    ident.idText = bindingIdent.idText && isSymbolMutable ident |> not
                | _ -> false
                
            findBindingIdentifier pattern |> Option.iter (fun bindingIdent ->
                if exprIdentMatchesBindingIdent bindingIdent expr then
                    visitorParameters.VisitorInfo.PostError 
                        range 
                        (FSharpLint.Framework.Resources.GetString("RulesUselessBindingError")))

    let checkTupleOfWildcards visitorParameters pattern identifier =
        let isEnabled =
            "TupleOfWildcards" |> isRuleEnabled visitorParameters.VisitorInfo.Config &&
            visitorParameters.AstNode.IsSuppressed(AnalyserName, "TupleOfWildcards") |> not

        if isEnabled then
            let rec isWildcard = function
                | SynPat.Paren(pattern, _) -> isWildcard pattern
                | SynPat.Wild(_) -> true
                | _ -> false

            let constructorString numberOfWildcards =
                let constructorName = identifier |> String.concat "."
                let arguments = [ for i in 1..numberOfWildcards -> "_" ] |> String.concat ", "
                constructorName + "(" + arguments + ")"

            match pattern with
                | SynPat.Tuple(patterns, range) when List.length patterns > 1 && patterns |> List.forall isWildcard ->
                    let errorFormat = FSharpLint.Framework.Resources.GetString("RulesTupleOfWildcardsError")
                    let refactorFrom, refactorTo = constructorString(List.length patterns), constructorString 1
                    let error = System.String.Format(errorFormat, refactorFrom, refactorTo)
                    visitorParameters.VisitorInfo.PostError range error
                | _ -> ()
    
    let visitor visitorInfo checkFile (astNode:CurrentNode) = 
        let visitorParameters = 
            { 
                VisitorInfo = visitorInfo
                CheckFile = checkFile
                AstNode = astNode
            }

        match astNode.Node with
            | AstNode.Binding(SynBinding.Binding(_, _, _, isMutable, _, _, _, pattern, _, expr, range, _)) -> 
                checkForBindingToAWildcard visitorParameters pattern range
                if not isMutable then
                    checkForUselessBinding visitorParameters pattern expr range
            | AstNode.Pattern(SynPat.Named(SynPat.Wild(_), _, _, _, _) as pattern) ->
                checkForWildcardNamedWithAsPattern visitorParameters pattern
            | AstNode.Pattern(SynPat.LongIdent(identifier, _, _, SynConstructorArgs.Pats([SynPat.Paren(SynPat.Tuple(_) as pattern, _)]), _, _)) ->
                let identifier = identifier.Lid |> List.map (fun x -> x.idText)
                checkTupleOfWildcards visitorParameters pattern identifier
            | _ -> ()

        Continue

    type RegisterBindingVisitor() = 
        let plugin =
            {
                Name = AnalyserName
                Visitor = Ast(visitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin