module FSharpLint.Rules.TupleOfWildcards

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let rule =
    let runner (args:AstNodeRuleParams) =
        let checkTupleOfWildcards fileContents pattern identifier identifierRange =
            let rec isWildcard = function
                | SynPat.Paren(innerPattern, _) -> isWildcard innerPattern
                | SynPat.Wild(_) -> true
                | _ -> false

            let constructorString numberOfWildcards =
                let constructorName = String.concat "." identifier
                let arguments = Array.create numberOfWildcards "_" |> String.concat ", "
                if numberOfWildcards = 1 then
                    $"%s{constructorName} _"
                else
                    $"%s{constructorName}(%s{arguments})"

            match pattern with
            | SynPat.Tuple(_isStruct, patterns, _, range) when List.length patterns > 1 && patterns |> List.forall isWildcard ->
                let errorFormat = Resources.GetString("RulesTupleOfWildcardsError")
                let refactorFrom = constructorString (List.length patterns)
                let refactorTo = constructorString 1
                let error = String.Format(errorFormat, refactorFrom, refactorTo)
                let suggestedFix = lazy(
                    Some { SuggestedFix.FromRange = identifierRange; FromText = fileContents; ToText = refactorTo })
                Array.singleton
                    {
                        Range = range
                        Message = error
                        SuggestedFix = Some suggestedFix
                        TypeChecks = List.Empty
                    }
            | _ -> Array.empty

        let isTupleMemberArgs breadcrumbs tupleRange =
            let (|MemberBindingArgs|_|) bindingPattern =
                match bindingPattern with
                | SynBinding(_, _, _, _, _, _, _, SynPat.LongIdent(_, _, _, argPats, _, _), _, _, _, _, _) ->
                    match argPats with
                    | SynArgPats.Pats([SynPat.Paren(SynPat.Tuple(_) as tuple, _)]) -> Some(tuple)
                    | _ -> None
                | _ -> None

            match breadcrumbs with
            | AstNode.Binding(MemberBindingArgs(SynPat.Tuple(_, _, _, range)))::AstNode.Expression(SynExpr.ObjExpr(_))::_
            | AstNode.Binding(MemberBindingArgs(SynPat.Tuple(_, _, _, range)))::AstNode.MemberDefinition(_)::_ ->
                tupleRange = range
            | _ -> false

        match args.AstNode with
        | AstNode.Pattern(SynPat.LongIdent(identifier, _, _, SynArgPats.Pats([SynPat.Paren(SynPat.Tuple(_, _, _, range) as pattern, _)]), _, identRange)) ->
            let breadcrumbs = args.GetParents 2
            if (not << isTupleMemberArgs breadcrumbs) range then
                let identifier = identifier.LongIdent |> List.map (fun ident -> ident.idText)
                checkTupleOfWildcards args.FileContent pattern identifier identRange
            else
                Array.empty
        | _ -> Array.empty

    AstNodeRule
        {
            Name = "TupleOfWildcards"
            Identifier = Identifiers.TupleOfWildcards
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
