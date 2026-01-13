module FSharpLint.Rules.DisallowShadowing

open System
open FSharp.Compiler.CodeAnalysis
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

        let rangeIncludedsDefinitions (definitions: array<FSharpSymbolUse>) range =
            definitions
            |> Array.exists (fun usage -> ExpressionUtilities.rangeContainsOtherRange range usage.Range)

        let rangeIncludedsDefinitionsBeforeCurrent =
            rangeIncludedsDefinitions definitionsBeforeCurrent
        
        let processBinding (binding: SynBinding) =
            if ExpressionUtilities.rangeContainsOtherRange binding.RangeOfBindingWithRhs identifier.idRange then
                // inside binding's scope
                rangeIncludedsDefinitionsBeforeCurrent binding.RangeOfHeadPattern
            else
                // outside binding's scope
                match binding with
                | SynBinding(_, _, _, _, _, _, _, headPat, _, _, _, _, _) ->
                    match headPat with
                    | SynPat.Named(SynIdent(ident, _), _, _, _) -> rangeIncludedsDefinitionsBeforeCurrent ident.idRange
                    | SynPat.LongIdent(SynLongIdent([ ident ], _, _), _, _, _, _, _) -> 
                        rangeIncludedsDefinitionsBeforeCurrent ident.idRange
                    | _ -> false

        let processArgs (args: SynSimplePats) =
            args
            |> extractIdentifiersFromSimplePats
            |> List.exists (fun ident -> rangeIncludedsDefinitionsBeforeCurrent ident.idRange)

        let rec processExpression (expression: SynExpr) =
            match expression with
            | SynExpr.LetOrUse(_, _, bindings, _, _, _) ->
                bindings |> List.exists processBinding
            | SynExpr.Sequential(_, _, expr1, expr2, _, _) ->
                processExpression expr1 || processExpression expr2
            | SynExpr.Lambda(_, _, args, body, _, _, _) ->
                processExpression body || processArgs args
            | _ -> false

        let rec processPattern (definitions: array<FSharpSymbolUse>) (pattern: SynPat) =
            match pattern with
            | SynPat.Named(SynIdent(ident, _), _, _, _) -> rangeIncludedsDefinitions definitions ident.idRange
            | SynPat.Ands(pats, _) -> pats |> List.exists (processPattern definitions)
            | SynPat.Or(lhs, rhs, _, _) ->
                let definitionsExcludingLhs =
                    definitions
                    |> Array.filter (fun usage -> not <| ExpressionUtilities.rangeContainsOtherRange lhs.Range usage.Range)
                processPattern definitionsExcludingLhs lhs || processPattern definitionsExcludingLhs rhs
            | SynPat.ArrayOrList(_, pats, _) -> pats |> List.exists (processPattern definitions)
            | SynPat.As(_lhs, rhs, _) -> processPattern definitions rhs
            | SynPat.OptionalVal(ident, _) -> rangeIncludedsDefinitions definitions ident.idRange
            | SynPat.Paren(pat, _) -> processPattern definitions pat
            | SynPat.Record(fieldPats, _) -> fieldPats |> List.exists (fun (_, _, pat) -> processPattern definitions pat)
            | SynPat.Tuple(_, pats, _, _) -> pats |> List.exists (processPattern definitions)
            | SynPat.Typed(pat, _, _) -> processPattern definitions pat
            | _ -> false

        let processModuleDeclaration (moduleDecl: SynModuleDecl) =
            match moduleDecl with
            | SynModuleDecl.Let(_, bindings, _) ->
                bindings 
                |> List.exists processBinding
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
                processPattern definitionsBeforeCurrent pattern
            | MemberDefinition(SynMemberDefn.Member(memberDefn, _)) ->
                processBinding memberDefn
            | TypeDefinition(SynTypeDefn(_, _, _, Some(SynMemberDefn.ImplicitCtor(_, _, ctorArgs, _, _, _, _)), _, _)) ->
                processPattern definitionsBeforeCurrent ctorArgs
            | _ -> false

        let parents = args.GetParents args.NodeIndex
        let isShadowing = 
            let currentDefinition = 
                definitionsWithSameName |> Seq.tryFind (fun definition -> definition.Range = identifier.idRange)
            match currentDefinition with
            | Some _ ->
                parents |> List.exists processAstNode
            | None ->
                false

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
