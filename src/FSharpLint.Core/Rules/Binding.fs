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

namespace FSharpLint.Rules

module Binding =

    open System
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.Analyser

    module Analysis =
        type IBindingAnalyser =
            abstract member FavourIgnoreOverLetWild: Rule
            abstract member WildcardNamedWithAsPattern: Rule
            abstract member UselessBinding: Rule
            abstract member TupleOfWildcards: Rule

        /// Checks if any code uses 'let _ = ...' and suggests to use the ignore function.
        let checkForBindingToAWildcard analysisArgs pattern range (analyser:IBindingAnalyser) i =
            let rule = analyser.FavourIgnoreOverLetWild
            if rule.Enabled then
                let rec findWildAndIgnoreParens = function
                    | SynPat.Paren(pattern, _) -> findWildAndIgnoreParens pattern
                    | SynPat.Wild(_) -> true
                    | _ -> false
                
                if findWildAndIgnoreParens pattern && rule.NotSuppressed analysisArgs i then
                    let error = rule.MessageFormat()
                    analysisArgs.Context.PostError range error

        let checkForWildcardNamedWithAsPattern analysisArgs pattern (analyser:IBindingAnalyser) i =
            let rule = analyser.WildcardNamedWithAsPattern
            if rule.Enabled then
                match pattern with
                | SynPat.Named(SynPat.Wild(wildcardRange), _, _, _, range) when wildcardRange <> range ->
                    if rule.NotSuppressed analysisArgs i then
                        let error = rule.MessageFormat()
                        analysisArgs.Context.PostError range error
                | _ -> ()

        let checkForUselessBinding analysisArgs pattern expr range (analyser:IBindingAnalyser) i =
            let rule = analyser.UselessBinding
            match analysisArgs.CheckResults with
            | Some(checkFile) when rule.Enabled ->
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
                                checkFile.GetSymbolUseAtLocation(ident.idRange.StartLine, ident.idRange.EndColumn, "", [ident.idText])
                                |> Async.RunSynchronously

                            let isMutable (symbol:FSharpSymbolUse) = 
                                match symbol.Symbol with
                                | :? FSharpMemberOrFunctionOrValue as v -> v.IsMutable
                                | _ -> false

                            symbol |> Option.exists isMutable

                        ident.idText = bindingIdent.idText && isSymbolMutable ident |> not
                    | _ -> false

                findBindingIdentifier pattern |> Option.iter (fun bindingIdent ->
                    if exprIdentMatchesBindingIdent bindingIdent expr && rule.NotSuppressed analysisArgs i then
                        let error = rule.MessageFormat()
                        analysisArgs.Context.PostError range error)
            | _ -> ()

        let checkTupleOfWildcards analysisArgs pattern identifier (analyser:IBindingAnalyser) i =
            let rule = analyser.TupleOfWildcards
            if rule.Enabled then
                let rec isWildcard = function
                    | SynPat.Paren(pattern, _) -> isWildcard pattern
                    | SynPat.Wild(_) -> true
                    | _ -> false

                let constructorString numberOfWildcards =
                    let constructorName = identifier |> String.concat "."
                    let arguments = Array.create numberOfWildcards "_" |> String.concat ", "
                    constructorName + "(" + arguments + ")"

                match pattern with
                | SynPat.Tuple(patterns, range) when List.length patterns > 1 && patterns |> List.forall isWildcard ->
                    if rule.NotSuppressed analysisArgs i then
                        let refactorFrom, refactorTo = constructorString(List.length patterns), constructorString 1
                        let error = rule.MessageFormat(refactorFrom, refactorTo)
                        analysisArgs.Context.PostError range error
                | _ -> ()

        let private isLetBinding i (syntaxArray:AbstractSyntaxArray.Node []) (skipArray:AbstractSyntaxArray.Skip []) =
            if i > 0 then
                match syntaxArray.[skipArray.[i].ParentIndex].Actual with 
                | AstNode.ModuleDeclaration(SynModuleDecl.Let(_))
                | AstNode.Expression(SynExpr.LetOrUse(_, false, _, _, _)) -> true
                | _ -> false
            else false

        let isTupleMemberArgs breadcrumbs tupleRange =
            let (|MemberBindingArgs|_|) bindingPattern =
                match bindingPattern with
                | SynBinding.Binding(_, _, _, _, _, _, _, SynPat.LongIdent(_, _, _, args, _, _), _, _, _, _) ->
                    match args with
                    | SynConstructorArgs.Pats([SynPat.Paren(SynPat.Tuple(_) as args, _)]) -> Some(args)
                    | _ -> None
                | _ -> None

            match breadcrumbs with 
            | AstNode.Binding(MemberBindingArgs(SynPat.Tuple(_, range)))::AstNode.Expression(SynExpr.ObjExpr(_))::_
            | AstNode.Binding(MemberBindingArgs(SynPat.Tuple(_, range)))::AstNode.MemberDefinition(_)::_ -> 
                tupleRange = range
            | _ -> false

        let analyse (analyser:IBindingAnalyser) analysisArgs = 
            let mutable i = 0
            while i < analysisArgs.SyntaxArray.Length do
                match analysisArgs.SyntaxArray.[i].Actual with
                | AstNode.Binding(SynBinding.Binding(_, _, _, isMutable, _, _, _, pattern, _, expr, range, _)) 
                        when isLetBinding i analysisArgs.SyntaxArray analysisArgs.SkipArray ->
                    checkForBindingToAWildcard analysisArgs pattern range analyser i
                    if not isMutable then
                        checkForUselessBinding analysisArgs pattern expr range analyser i
                | AstNode.Pattern(SynPat.Named(SynPat.Wild(_), _, _, _, _) as pattern) ->
                    checkForWildcardNamedWithAsPattern analysisArgs pattern analyser i
                | AstNode.Pattern(SynPat.LongIdent(identifier, _, _, SynConstructorArgs.Pats([SynPat.Paren(SynPat.Tuple(_, range) as pattern, _)]), _, _)) ->
                    let breadcrumbs = getBreadcrumbs 2 analysisArgs i
                    if (not << isTupleMemberArgs breadcrumbs) range then
                        let identifier = identifier.Lid |> List.map (fun x -> x.idText)
                        checkTupleOfWildcards analysisArgs pattern identifier analyser i
                | _ -> ()

                i <- i + 1

    [<Sealed>]
    type BindingAnalyser(config) =
        inherit Analyser.Analyser(name = "Binding", code = "1", config = config)

        interface Analysis.IBindingAnalyser with
            member this.FavourIgnoreOverLetWild = 
                this.Rule(ruleName = "FavourIgnoreOverLetWild", code = "1", ruleConfig = config)

            member this.WildcardNamedWithAsPattern = 
                this.Rule(ruleName = "WildcardNamedWithAsPattern", code = "2", ruleConfig = config)

            member this.UselessBinding = 
                this.Rule(ruleName = "UselessBinding", code = "3", ruleConfig = config)

            member this.TupleOfWildcards = 
                this.Rule(ruleName = "TupleOfWildcards", code = "4", ruleConfig = config)

        override this.Analyse analysisArgs = 
            Analysis.analyse this analysisArgs