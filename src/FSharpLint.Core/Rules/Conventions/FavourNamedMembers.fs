module FSharpLint.Rules.FavourNamedMembers

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

[<TailCall>]
let rec private isAllWildcards (patterns: list<SynPat>) =
    match patterns with
    | [] -> 
        // reached end of the list (initially function is called on non-empty list)
        true
    | SynPat.Wild _ :: rest -> isAllWildcards rest
    | SynPat.Paren(pat, _) :: rest -> isAllWildcards (pat :: rest)
    | SynPat.Tuple(_, elementPats, _, _) :: rest -> isAllWildcards (elementPats @ rest)
    | _ -> false

let runner (args: AstNodeRuleParams) =
    let emitWarning range =
        Array.singleton 
            { 
                Range = range
                Message = Resources.GetString "RulesFavourNamedMembers"
                SuggestedFix = None
                TypeChecks = List.Empty 
            }

    let getFields (case: SynUnionCase) =
        match case with 
        | SynUnionCase(_, _, SynUnionCaseKind.Fields(fields), _, _, _, _) ->
            fields
        | _ -> List.empty

    let isNamedFieldDeclaration (field: SynField) =
        match field with
        | SynField(_, false, Some(_), _, false, _, _, _, _) -> true
        | _ -> false

    match args.AstNode with
    | AstNode.TypeSimpleRepresentation(SynTypeDefnSimpleRepr.Union(_, cases, _)) ->
        let fields =
            cases
            |> List.collect getFields

        if cases.Length = 1 && fields.Length = 1 then
            Array.empty
        else
            fields
            |> Array.ofList
            |> Array.filter (not << isNamedFieldDeclaration)
            |> Array.collect (fun field -> emitWarning field.Range)
    | AstNode.Match(SynMatchClause(SynPat.LongIdent(ident, _, _, argPats, _, range), _, _, _, _, _)) ->
        match argPats with
        | SynArgPats.Pats(pats) when pats.Length >= 1 && not (isAllWildcards pats) ->
            match args.CheckInfo with
            | Some checkInfo ->
                let maybeDuCaseSymbolUse =
                    checkInfo.GetSymbolUseAtLocation(
                        ident.Range.StartLine, 
                        ident.Range.EndColumn, 
                        args.Lines.[ident.Range.StartLine], 
                        ident.LongIdent |> List.map (fun id -> id.idText)
                    )

                match maybeDuCaseSymbolUse with
                | Some duCaseSymbolUse ->
                    let maybeDuCaseDeclarationSymbol =
                        checkInfo.GetAllUsesOfAllSymbolsInFile()
                        |> Seq.tryFind 
                            (fun symbolUse -> 
                                symbolUse.IsFromDefinition 
                                && symbolUse.Symbol.IsEffectivelySameAs duCaseSymbolUse.Symbol)

                    match maybeDuCaseDeclarationSymbol with
                    | Some duCaseDeclarationSymbol ->
                        let duCaseHasNamedFields =
                            args.SyntaxArray
                            |> Array.exists 
                                (fun node -> 
                                    match node.Actual with
                                    | AstNode.UnionCase(SynUnionCase(_, unionCaseIdent, _, _, _, _, _) as unionCase)
                                        when unionCaseIdent.Range = duCaseDeclarationSymbol.Range ->
                                        getFields unionCase
                                        |> List.exists isNamedFieldDeclaration
                                    | _ -> false)

                        if duCaseHasNamedFields then
                            emitWarning range
                        else
                            Array.empty
                    | None -> Array.empty
                | None -> Array.empty
            | None -> Array.empty
        | _ ->
            Array.empty
    | _ ->
        Array.empty

let rule =
    AstNodeRule
        {
            Name = "FavourNamedMembers"
            Identifier = Identifiers.FavourNamedMembers
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
