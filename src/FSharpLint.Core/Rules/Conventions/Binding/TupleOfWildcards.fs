module FSharpLint.Rules.TupleOfWildcards

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private checkTupleOfWildcards pattern identifier =
    let rec isWildcard = function
        | SynPat.Paren(pattern, _) -> isWildcard pattern
        | SynPat.Wild(_) -> true
        | _ -> false

    let constructorString numberOfWildcards =
        let constructorName = identifier |> String.concat "."
        let arguments = Array.create numberOfWildcards "_" |> String.concat ", "
        constructorName + "(" + arguments + ")"

    match pattern with
    | SynPat.Tuple(_isStruct, patterns, range) when List.length patterns > 1 && patterns |> List.forall isWildcard ->
        let errorFormat = Resources.GetString("RulesTupleOfWildcardsError")
        let refactorFrom = constructorString (List.length patterns)
        let refactorTo = (constructorString 1)
        let error = System.String.Format(errorFormat, refactorFrom, refactorTo)
        { Range = range; Message = error; SuggestedFix = None; TypeChecks = [] } |> Array.singleton
    | _ -> Array.empty

let private isTupleMemberArgs breadcrumbs tupleRange =
    let (|MemberBindingArgs|_|) bindingPattern =
        match bindingPattern with
        | SynBinding.Binding(_, _, _, _, _, _, _, SynPat.LongIdent(_, _, _, args, _, _), _, _, _, _) ->
            match args with
            | SynArgPats.Pats([SynPat.Paren(SynPat.Tuple(_) as args, _)]) -> Some(args)
            | _ -> None
        | _ -> None

    match breadcrumbs with
    | AstNode.Binding(MemberBindingArgs(SynPat.Tuple(_, _, range)))::AstNode.Expression(SynExpr.ObjExpr(_))::_
    | AstNode.Binding(MemberBindingArgs(SynPat.Tuple(_, _, range)))::AstNode.MemberDefinition(_)::_ ->
        tupleRange = range
    | _ -> false

let private runner (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Pattern(SynPat.LongIdent(identifier, _, _, SynArgPats.Pats([SynPat.Paren(SynPat.Tuple(_, _, range) as pattern, _)]), _, _)) ->
        let breadcrumbs = args.GetParents 2
        if (not << isTupleMemberArgs breadcrumbs) range then
            let identifier = identifier.Lid |> List.map (fun x -> x.idText)
            checkTupleOfWildcards pattern identifier
        else
            Array.empty
    | _ -> Array.empty

let rule =
    { Name = "TupleOfWildcards"
      Identifier = Identifiers.TupleOfWildcards
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
