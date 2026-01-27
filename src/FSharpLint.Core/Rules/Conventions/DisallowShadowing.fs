module FSharpLint.Rules.DisallowShadowing

open System
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

[<TailCall>]
let rec private extractIdentifier (pattern: SynSimplePat) =
    match pattern with
    | SynSimplePat.Id(ident, _, _, _, _, _) ->
        ident
    | SynSimplePat.Attrib(pat, _, _)
    | SynSimplePat.Typed(pat, _, _) ->
        extractIdentifier pat

let private rangeIncludesDefinitions (definitions: array<FSharpSymbolUse>) range =
    definitions
    |> Array.exists (fun usage -> ExpressionUtilities.rangeContainsOtherRange range usage.Range)

[<TailCall>]
let rec private processExpressions 
    (processBinding: SynBinding -> bool)
    (processArgs: SynSimplePats -> bool)
    (expressions: list<SynExpr>) =
    match expressions with
    | SynExpr.LetOrUse(_, _, _, false, bindings, _, _, _) :: rest ->
        bindings |> List.exists processBinding
        || processExpressions processBinding processArgs rest
    | SynExpr.Sequential(_, _, expr1, expr2, _, _) :: rest ->
        processExpressions processBinding processArgs (expr1 :: expr2 :: rest)
    | SynExpr.Lambda(_, _, arguments, body, _, _, _) :: rest ->
        processArgs arguments
        || processExpressions processBinding processArgs (body :: rest)
    | _ -> false

[<TailCall>]
let rec private processPatterns (definitionsAndPatterns: list<array<FSharpSymbolUse> * SynPat>) =
    match definitionsAndPatterns with
    | (definitions, SynPat.Named(SynIdent(ident, _), _, _, _)) :: rest -> 
        rangeIncludesDefinitions definitions ident.idRange
        || processPatterns rest
    | (definitions, SynPat.Ands(pats, _)) :: rest ->
        processPatterns ((pats |> List.map (fun pat -> (definitions, pat))) @ rest)
    | (definitions, SynPat.Or(lhs, rhs, _, _)) :: rest ->
        let definitionsExcludingLhs =
            definitions
            |> Array.filter (fun usage -> not <| ExpressionUtilities.rangeContainsOtherRange lhs.Range usage.Range)
        processPatterns ([ (definitionsExcludingLhs, lhs); (definitionsExcludingLhs, rhs) ] @ rest)
    | (definitions, SynPat.ArrayOrList(_, pats, _)) :: rest -> 
        processPatterns ((pats |> List.map (fun pat -> (definitions, pat))) @ rest)
    | (definitions, SynPat.As(_lhs, rhs, _)) :: rest -> 
        processPatterns ((definitions, rhs) :: rest)
    | (definitions, SynPat.OptionalVal(ident, _)) :: rest ->
        rangeIncludesDefinitions definitions ident.idRange
        || processPatterns rest
    | (definitions, SynPat.Paren(pat, _)) :: rest ->
        processPatterns ((definitions, pat) :: rest)
    | (definitions, SynPat.Record(fieldPats, _)) :: rest ->
        processPatterns ((fieldPats |> List.map (fun patPairField -> (definitions, patPairField.Pattern))) @ rest)
    | (definitions, SynPat.Tuple(_, pats, _, _)) :: rest ->
        processPatterns ((pats |> List.map (fun pat -> (definitions, pat))) @ rest)
    | (definitions, SynPat.Typed(pat, _, _)) :: rest ->
        processPatterns ((definitions, pat) :: rest)
    | _ -> false

let runner (args: AstNodeRuleParams) =
    let extractIdentifiersFromSimplePats (simplePats: SynSimplePats) : List<Ident> =
        match simplePats with
        | SynSimplePats.SimplePats(patterns, _, _) ->
            patterns |> List.map extractIdentifier

    let checkIdentifier (identifier: Ident) : array<WarningDetails> =
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

            let rangeIncludesDefinitionsBeforeCurrent =
                rangeIncludesDefinitions definitionsBeforeCurrent
        
            let processBinding (binding: SynBinding) =
                if ExpressionUtilities.rangeContainsOtherRange binding.RangeOfBindingWithRhs identifier.idRange then
                    // inside binding's scope
                    rangeIncludesDefinitionsBeforeCurrent binding.RangeOfHeadPattern
                else
                    // outside binding's scope
                    match binding with
                    | SynBinding(_, _, _, _, _, _, _, headPat, _, _, _, _, _) ->
                        match headPat with
                        | SynPat.Named(SynIdent(ident, _), _, _, _) -> rangeIncludesDefinitionsBeforeCurrent ident.idRange
                        | SynPat.LongIdent(SynLongIdent([ ident ], _, _), _, _, _, _, _) -> 
                            rangeIncludesDefinitionsBeforeCurrent ident.idRange
                        | _ -> false

            let processArgs (arguments: SynSimplePats) =
                arguments
                |> extractIdentifiersFromSimplePats
                |> List.exists (fun ident -> rangeIncludesDefinitionsBeforeCurrent ident.idRange)

            let processModuleDeclaration (moduleDecl: SynModuleDecl) =
                match moduleDecl with
                | SynModuleDecl.Let(_, bindings, _) -> bindings |> List.exists processBinding
                | _ -> false

            let processExpression (expr: SynExpr) = processExpressions processBinding processArgs (List.singleton expr)

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
                    processPatterns (List.singleton (definitionsBeforeCurrent, pattern))
                | MemberDefinition(SynMemberDefn.Member(memberDefn, _)) ->
                    processBinding memberDefn
                | TypeDefinition(SynTypeDefn(_, _, _, Some(SynMemberDefn.ImplicitCtor(_, _, ctorArgs, _, _, _, _)), _, _)) ->
                    processPatterns (List.singleton (definitionsBeforeCurrent, ctorArgs))
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

    match args.AstNode with
    | Pattern(SynPat.Named(SynIdent(ident, _), _, _, _)) ->
        checkIdentifier ident
    | Lambda(lambda, _) ->
        lambda.Arguments 
        |> List.collect extractIdentifiersFromSimplePats
        |> List.toArray
        |> Array.collect checkIdentifier
    | _ ->
        Array.empty

let rule =
    { Name = "DisallowShadowing"
      Identifier = Identifiers.DisallowShadowing
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
