module FSharpLint.Rules.DisallowShadowing

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private extractIdentifiersFromSimplePats (simplePats: SynSimplePats) : List<Ident> =
    let rec extractIdentifier (pattern: SynSimplePat) =
        match pattern with
        | SynSimplePat.Id(ident, _, _, _, _, _) ->
            ident
        | SynSimplePat.Attrib(pat, _, _)
        | SynSimplePat.Typed(pat, _, _) ->
            extractIdentifier pat

    match simplePats with
    | SynSimplePats.SimplePats(patterns, _, _) ->
        patterns |> List.map extractIdentifier


let private checkIdentifier (args: AstNodeRuleParams) (identifier: Ident) : array<WarningDetails> =
    let name = identifier.idText
    match args.CheckInfo with
    | Some checkResults when not (name.StartsWith '_') -> 
        let allUsages = checkResults.GetAllUsesOfAllSymbolsInFile()
        let definitionsWithSameName = 
            allUsages 
            |> Seq.filter (fun usage -> usage.IsFromDefinition && usage.Symbol.DisplayName = name)

        let definitionsBeforeCurrent =
            definitionsWithSameName
            |> Seq.filter 
                (fun usage -> 
                    (usage.Range.StartLine, usage.Range.StartColumn) 
                        < (identifier.idRange.StartLine, identifier.idRange.StartColumn) )
            |> Seq.toArray

        let rangeIncludedsDefinitions range =
            definitionsBeforeCurrent
            |> Array.exists (fun usage -> ExpressionUtilities.rangeContainsOtherRange range usage.Range)
        
        let processBinding binding =
            match binding with
            | SynBinding(_, _, _, _, _, _, _, _, _, _, range, _, _) ->
                rangeIncludedsDefinitions range

        let processArgs (args: SynSimplePats) =
            args
            |> extractIdentifiersFromSimplePats
            |> List.exists (fun ident -> rangeIncludedsDefinitions ident.idRange)

        let rec processExpression (expression: SynExpr) =
            match expression with
            | SynExpr.LetOrUse(_, _, bindings, _, _, _) ->
                bindings |> List.exists processBinding
            | SynExpr.Sequential(_, _, expr1, expr2, _, _) ->
                processExpression expr1 || processExpression expr2
            | SynExpr.Lambda(_, _, args, body, _, _, _) ->
                processExpression body || processArgs args
            | _ -> false

        let rec processPattern (pattern: SynPat) =
            match pattern with
            | SynPat.Named(SynIdent(ident, _), _, _, _) -> rangeIncludedsDefinitions ident.idRange
            | SynPat.Ands(pats, _) -> pats |> List.exists processPattern
            | SynPat.Or(lhs, rhs, _, _) -> processPattern lhs || processPattern rhs
            | SynPat.ArrayOrList(_, pats, _) -> pats |> List.exists processPattern
            | SynPat.As(_lhs, rhs, _) -> processPattern rhs
            | SynPat.OptionalVal(ident, _) -> rangeIncludedsDefinitions ident.idRange
            | SynPat.Paren(pat, _) -> processPattern pat
            | SynPat.Record(fieldPats, _) -> fieldPats |> List.exists (fun (_, _, pat) -> processPattern pat)
            | SynPat.Tuple(_, pats, _, _) -> pats |> List.exists processPattern
            | SynPat.Typed(pat, _, _) -> processPattern pat
            | _ -> false

        let processParentBinding (binding: SynBinding) =
            if ExpressionUtilities.rangeContainsOtherRange binding.RangeOfBindingWithRhs identifier.idRange then
                // inside binding's scope
                rangeIncludedsDefinitions binding.RangeOfHeadPattern
            else
                // outside binding's scope
                match binding with
                | SynBinding(_, _, _, _, _, _, _, headPat, _, _, _, _, _) ->
                    match headPat with
                    | SynPat.Named(SynIdent(ident, _), _, _, _) -> rangeIncludedsDefinitions ident.idRange
                    | SynPat.LongIdent(SynLongIdent(identParts, _, _), _, _, _, _, _) -> 
                        identParts |> List.exists (fun ident -> rangeIncludedsDefinitions ident.idRange)
                    | _ -> false

        let processModuleDeclaration (moduleDecl: SynModuleDecl) =
            match moduleDecl with
            | SynModuleDecl.Let(_, bindings, _) ->
                bindings 
                |> List.exists processParentBinding
            | _ -> false

        let processAstNode (node: AstNode) =
            match node with
            | ModuleOrNamespace(SynModuleOrNamespace(_, _, _, declarations, _, _, _, _, _)) ->
                declarations |> List.exists processModuleDeclaration
            | Expression(expression) ->
                processExpression (ExpressionUtilities.removeParens expression)
            | Lambda(lambda, _) ->
                processExpression lambda.Body
                || (lambda.Arguments |> List.exists processArgs)
            | Match(SynMatchClause(pattern, _, _, _, _, _)) ->
                processPattern pattern
            | MemberDefinition(SynMemberDefn.Member(memberDefn, _)) ->
                processBinding memberDefn
            | TypeDefinition(SynTypeDefn(_, _, _, Some(SynMemberDefn.ImplicitCtor(_, _, ctorArgs, _, _, _, _)), _, _)) ->
                processPattern ctorArgs
            | _ -> false

        let parents = args.GetParents args.NodeIndex
        let isShadowing =
            parents |> List.exists processAstNode

        if isShadowing then
            Array.singleton { 
                Range = identifier.idRange
                Message = Resources.GetString "RulesDisallowShadowing"
                SuggestedFix = None
                TypeChecks = List.Empty }
        else
            Array.empty
                
    | _ -> Array.empty

let runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | Pattern(SynPat.Named(SynIdent(ident, _), _, _, _)) ->
        checkIdentifier args ident
    | Lambda(lambda, _) ->
        lambda.Arguments 
        |> List.collect extractIdentifiersFromSimplePats
        |> List.toArray
        |> Array.collect (fun each -> checkIdentifier args each)
    | _ ->
        Array.empty

let rule =
    { Name = "DisallowShadowing"
      Identifier = Identifiers.DisallowShadowing
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
