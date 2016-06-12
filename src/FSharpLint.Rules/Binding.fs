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
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "Binding"

    let isRuleEnabled config ruleName =
        isRuleEnabled config AnalyserName ruleName |> Option.isSome

    type VisitorParameters =
        { VisitorInfo: VisitorInfo
          CheckFile: FSharpCheckFileResults option }
            
    /// Checks if any code uses 'let _ = ...' and suggests to use the ignore function.
    let checkForBindingToAWildcard visitorParameters pattern range isSuppressed =
        let ruleName = "FavourIgnoreOverLetWild"

        let isEnabled = isRuleEnabled visitorParameters.VisitorInfo.Config ruleName

        if isEnabled then
            let rec findWildAndIgnoreParens = function
                | SynPat.Paren(pattern, _) -> findWildAndIgnoreParens pattern
                | SynPat.Wild(_) -> true
                | _ -> false
                
            if findWildAndIgnoreParens pattern && isSuppressed ruleName |> not then
                visitorParameters.VisitorInfo.PostError 
                    range 
                    (Resources.GetString("RulesFavourIgnoreOverLetWildError"))

    let checkForWildcardNamedWithAsPattern visitorParameters pattern isSuppressed =
        let ruleName = "WildcardNamedWithAsPattern"

        let isEnabled = isRuleEnabled visitorParameters.VisitorInfo.Config ruleName

        if isEnabled then
            match pattern with
            | SynPat.Named(SynPat.Wild(wildcardRange), _, _, _, range) when wildcardRange <> range ->
                if isSuppressed ruleName |> not then
                    visitorParameters.VisitorInfo.PostError 
                        range 
                        (Resources.GetString("RulesWildcardNamedWithAsPattern"))
            | _ -> ()

    let checkForUselessBinding visitorParameters pattern expr range isSuppressed =
        let ruleName = "UselessBinding"

        let isEnabled = isRuleEnabled visitorParameters.VisitorInfo.Config ruleName
            
        match visitorParameters.CheckFile with
        | Some(checkFile) when isEnabled ->
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
                if exprIdentMatchesBindingIdent bindingIdent expr && isSuppressed ruleName |> not then
                    visitorParameters.VisitorInfo.PostError 
                        range 
                        (Resources.GetString("RulesUselessBindingError")))
            | _ -> ()

    let checkTupleOfWildcards visitorParameters pattern identifier isSuppressed =
        let ruleName = "TupleOfWildcards"

        let isEnabled = isRuleEnabled visitorParameters.VisitorInfo.Config ruleName

        if isEnabled then
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
                if isSuppressed ruleName |> not then
                    let errorFormat = Resources.GetString("RulesTupleOfWildcardsError")
                    let refactorFrom, refactorTo = constructorString(List.length patterns), constructorString 1
                    let error = System.String.Format(errorFormat, refactorFrom, refactorTo)
                    visitorParameters.VisitorInfo.PostError range error
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

    let analyser visitorInfo checkFile (syntaxArray:AbstractSyntaxArray.Node []) skipArray = 
        let visitorParameters =
            { VisitorInfo = visitorInfo
              CheckFile = checkFile }

        let isSuppressed i ruleName =
            AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
            |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName

        let mutable i = 0
        while i < syntaxArray.Length do
            match syntaxArray.[i].Actual with
            | AstNode.Binding(SynBinding.Binding(_, _, _, isMutable, _, _, _, pattern, _, expr, range, _)) 
                    when isLetBinding i syntaxArray skipArray ->
                checkForBindingToAWildcard visitorParameters pattern range (isSuppressed i)
                if not isMutable then
                    checkForUselessBinding visitorParameters pattern expr range (isSuppressed i)
            | AstNode.Pattern(SynPat.Named(SynPat.Wild(_), _, _, _, _) as pattern) ->
                checkForWildcardNamedWithAsPattern visitorParameters pattern (isSuppressed i)
            | AstNode.Pattern(SynPat.LongIdent(identifier, _, _, SynConstructorArgs.Pats([SynPat.Paren(SynPat.Tuple(_, range) as pattern, _)]), _, _)) ->
                let breadcrumbs = AbstractSyntaxArray.getBreadcrumbs 2 syntaxArray skipArray i
                if (not << isTupleMemberArgs breadcrumbs) range then
                    let identifier = identifier.Lid |> List.map (fun x -> x.idText)
                    checkTupleOfWildcards visitorParameters pattern identifier (isSuppressed i)
            | _ -> ()

            i <- i + 1