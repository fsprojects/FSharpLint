module FSharpLint.Rules.NoPartialFunctions

open System
open System.Linq
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax

[<RequireQualifiedAccess>]
type Config = {
    AllowedPartials:string list
    AdditionalPartials:string list
}

type private Replacement =
    | PatternMatch
    | Function of functionName:string

let private partialFunctionIdentifiers =
    Map.ofList
        [
            // Option
            ("Option.get", PatternMatch)

            // Map
            ("Map.find", Function "Map.tryFind")
            ("Map.findKey", Function "Map.tryFindKey")

            // Array
            ("Array.exactlyOne", Function "Array.tryExactlyOne")
            ("Array.get", Function "Array.tryItem")
            ("Array.item", Function "Array.tryItem")
            ("Array.find", Function "Array.tryFind")
            ("Array.findIndex", Function "Array.tryFindIndex")
            ("Array.findBack", Function "Array.tryFindBack")
            ("Array.head", Function "Array.tryHead")
            ("Array.last", Function "Array.tryLast")
            ("Array.tail", Function "FSharpx.Collections.Seq.tryHeadTail")
            ("Array.reduce", Function "Array.fold")
            ("Array.reduceBack", Function "Array.foldBack")
            ("Array.pick", Function "Array.tryPick")

            // Seq
            ("Seq.exactlyOne", Function "Seq.tryExactlyOne")
            ("Seq.item", Function "Seq.tryItem")
            ("Seq.find", Function "Seq.tryFind")
            ("Seq.findIndex", Function "Seq.tryFindIndex")
            ("Seq.findBack", Function "Seq.tryFindBack")
            ("Seq.head", Function "Seq.tryHead")
            ("Seq.last", Function "Seq.tryLast")
            ("Seq.tail", Function "FSharpx.Collections.Seq.tryHeadTail")
            ("Seq.reduce", Function "Seq.fold")
            ("Seq.reduceBack", Function "Seq.foldBack")
            ("Seq.pick", Function "Seq.tryPick")

            // List
            ("List.exactlyOne", Function "List.tryExactlyOne")
            ("List.item", Function "List.tryItem")
            ("List.find", Function "List.tryFind")
            ("List.findIndex", Function "List.tryFindIndex")
            ("List.findBack", Function "List.tryFindBack")
            ("List.head", Function "List.tryHead")
            ("List.last", Function "List.tryLast")
            ("List.tail", Function "FSharpx.Collections.Seq.tryHeadTail")
            ("List.reduce", Function "List.fold")
            ("List.reduceBack", Function "List.foldBack")
            ("List.pick", Function "List.tryPick")
        ]

/// List of tuples (fully qualified instance member name, namespace, argument compiled type name, replacement strategy)
let private partialInstanceMemberIdentifiers =
    [
        // see https://stackoverflow.com/a/70282499/544947
        ("Option.Value", Some "Microsoft.FSharp.Core", "option`1", PatternMatch)
        ("Map.Item", Some "Microsoft.FSharp.Collections", "FSharpMap`2", Function "Map.tryFind")
        ("List.Item", Some "Microsoft.FSharp.Collections", "list`1", Function "List.tryFind")
        ("List.Head", Some "Microsoft.FSharp.Collections", "list`1", Function "List.tryHead")

        // As an example for future additions (see commented Foo.Bar.Baz tests)
        //("Foo.Bar.Baz", None, "string", PatternMatch)
    ]

let private checkIfPartialIdentifier (config:Config) (identifier:string) (range:Range) =
    if List.contains identifier config.AllowedPartials then
        None
    elif List.contains identifier config.AdditionalPartials then
        Some {
            Range = range
            Message = String.Format(Resources.GetString ("RulesConventionsNoPartialFunctionsAdditionalError"), identifier)
            SuggestedFix = None
            TypeChecks = List.Empty
        }
    else
        Map.tryFind identifier partialFunctionIdentifiers
        |> Option.filter (fun _ -> not (List.contains identifier config.AllowedPartials))
        |> Option.map (function
            | PatternMatch ->
                {
                    Range = range
                    Message = String.Format(Resources.GetString ("RulesConventionsNoPartialFunctionsPatternMatchError"), identifier)
                    SuggestedFix = None
                    TypeChecks = List.Empty
                }
            | Function replacementFunction ->
                {
                    Range = range
                    Message = String.Format(Resources.GetString "RulesConventionsNoPartialFunctionsReplacementError", replacementFunction, identifier)
                    SuggestedFix = Some (lazy ( Some { FromText = identifier; FromRange = range; ToText = replacementFunction }))
                    TypeChecks = List.Empty
                })

let rec private tryFindTypedExpression (range: Range) (expression: FSharpExpr) = 
    let tryFindFirst exprs = 
        exprs |> Seq.choose (tryFindTypedExpression range) |> Seq.tryHead
    if expression.Range = range then
        Some expression
    else
        match expression with 
        | FSharpExprPatterns.AddressOf(lvalueExpr) -> 
            tryFindTypedExpression range lvalueExpr
        | FSharpExprPatterns.AddressSet(lvalueExpr, rvalueExpr) -> 
            tryFindTypedExpression range lvalueExpr |> Option.orElse (tryFindTypedExpression range rvalueExpr)
        | FSharpExprPatterns.Application(funcExpr, _typeArgs, argExprs) -> 
            tryFindFirst (funcExpr :: argExprs)
        | FSharpExprPatterns.Call(objExprOpt, _memberOrFunc, _typeArgs1, _typeArgs2, argExprs) ->
            tryFindFirst (List.append (Option.toList objExprOpt) argExprs)
        | FSharpExprPatterns.Coerce(_targetType, inpExpr) -> 
            tryFindTypedExpression range inpExpr
        | FSharpExprPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, _isUp, _, _) -> 
            tryFindFirst [ startExpr; limitExpr; consumeExpr ]
        | FSharpExprPatterns.ILAsm(_asmCode, _typeArgs, argExprs) -> 
            tryFindFirst argExprs
        | FSharpExprPatterns.ILFieldGet (objExprOpt, _fieldType, _fieldName) -> 
            objExprOpt |> Option.bind (tryFindTypedExpression range)
        | FSharpExprPatterns.ILFieldSet (objExprOpt, _fieldType, _fieldName, valueExpr) -> 
            objExprOpt |> Option.bind (tryFindTypedExpression range) |> Option.orElse (tryFindTypedExpression range valueExpr)
        | FSharpExprPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> 
            tryFindFirst [ guardExpr; thenExpr; elseExpr ]
        | FSharpExprPatterns.Lambda(_lambdaVar, bodyExpr) -> 
            tryFindTypedExpression range bodyExpr
        | FSharpExprPatterns.Let((_bindingVar, bindingExpr, _), bodyExpr) -> 
            tryFindTypedExpression range bindingExpr |> Option.orElse (tryFindTypedExpression range bodyExpr)
        | FSharpExprPatterns.LetRec(recursiveBindings, bodyExpr) ->
            recursiveBindings 
            |> Seq.choose (fun (_, expr, _) -> tryFindTypedExpression range expr)
            |> Seq.tryHead
            |> Option.orElse (tryFindTypedExpression range bodyExpr)
        | FSharpExprPatterns.NewArray(_arrayType, argExprs) -> 
            tryFindFirst argExprs
        | FSharpExprPatterns.NewDelegate(_delegateType, delegateBodyExpr) -> 
            tryFindTypedExpression range delegateBodyExpr
        | FSharpExprPatterns.NewObject(_objType, _typeArgs, argExprs) -> 
            tryFindFirst argExprs
        | FSharpExprPatterns.NewRecord(_recordType, argExprs) ->  
            tryFindFirst argExprs
        | FSharpExprPatterns.NewAnonRecord(_recordType, argExprs) ->  
            tryFindFirst argExprs
        | FSharpExprPatterns.NewTuple(_tupleType, argExprs) -> 
            tryFindFirst argExprs
        | FSharpExprPatterns.NewUnionCase(_unionType, _unionCase, argExprs) -> 
            tryFindFirst argExprs
        | FSharpExprPatterns.Quote(quotedExpr) -> 
            tryFindTypedExpression range quotedExpr
        | FSharpExprPatterns.FSharpFieldGet(objExprOpt, _recordOrClassType, _fieldInfo) -> 
            objExprOpt |> Option.bind (tryFindTypedExpression range)
        | FSharpExprPatterns.AnonRecordGet(objExpr, _recordOrClassType, _fieldInfo) -> 
            tryFindTypedExpression range objExpr
        | FSharpExprPatterns.FSharpFieldSet(objExprOpt, _recordOrClassType, _fieldInfo, argExpr) -> 
            objExprOpt |> Option.bind (tryFindTypedExpression range) |> Option.orElse (tryFindTypedExpression range argExpr)
        | FSharpExprPatterns.Sequential(firstExpr, secondExpr) -> 
            tryFindTypedExpression range firstExpr |> Option.orElse (tryFindTypedExpression range secondExpr)
        | FSharpExprPatterns.TryFinally(bodyExpr, finalizeExpr, _, _) -> 
            tryFindTypedExpression range bodyExpr |> Option.orElse (tryFindTypedExpression range finalizeExpr)
        | FSharpExprPatterns.TryWith(bodyExpr, _, _, _catchVar, catchExpr, _, _) -> 
            tryFindTypedExpression range bodyExpr |> Option.orElse (tryFindTypedExpression range catchExpr)
        | FSharpExprPatterns.TupleGet(_tupleType, _tupleElemIndex, tupleExpr) -> 
            tryFindTypedExpression range tupleExpr
        | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) -> 
            tryFindTypedExpression range decisionExpr
            |> Option.orElse (decisionTargets |> Seq.choose (fun (_, expr) -> tryFindTypedExpression range expr) |> Seq.tryHead)
        | FSharpExprPatterns.DecisionTreeSuccess (_decisionTargetIdx, decisionTargetExprs) -> 
            tryFindFirst decisionTargetExprs
        | FSharpExprPatterns.TypeLambda(_genericParam, bodyExpr) -> 
            tryFindTypedExpression range bodyExpr
        | FSharpExprPatterns.TypeTest(_ty, inpExpr) -> 
            tryFindTypedExpression range inpExpr
        | FSharpExprPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) -> 
            tryFindTypedExpression range unionExpr |> Option.orElse (tryFindTypedExpression range valueExpr)
        | FSharpExprPatterns.UnionCaseGet(unionExpr, _unionType, _unionCase, _unionCaseField) -> 
            tryFindTypedExpression range unionExpr
        | FSharpExprPatterns.UnionCaseTest(unionExpr, _unionType, _unionCase) -> 
            tryFindTypedExpression range unionExpr
        | FSharpExprPatterns.UnionCaseTag(unionExpr, _unionType) -> 
            tryFindTypedExpression range unionExpr
        | FSharpExprPatterns.ObjectExpr(_objType, baseCallExpr, overrides, interfaceImplementations) -> 
            let interfaceImlps = interfaceImplementations |> List.collect snd
            baseCallExpr :: (List.append overrides interfaceImlps |> Seq.cast<FSharpExpr> |> Seq.toList)
            |> tryFindFirst
        | FSharpExprPatterns.TraitCall(_sourceTypes, _traitName, _typeArgs, _typeInstantiation, _argTypes, argExprs) -> 
            tryFindFirst argExprs
        | FSharpExprPatterns.ValueSet(_valToSet, valueExpr) -> 
            tryFindTypedExpression range valueExpr
        | FSharpExprPatterns.WhileLoop(guardExpr, bodyExpr, _) -> 
            tryFindTypedExpression range guardExpr |> Option.orElse (tryFindTypedExpression range bodyExpr)
        | _ -> None

let private getTypedExpressionForRange (checkFile:FSharpCheckFileResults) (range: Range) =
    let expressions =
        match checkFile.ImplementationFile with
        | Some implementationFile ->
            let rec getExpressions declarations =
                seq {
                    for declaration in declarations do
                        match declaration with
                        | FSharpImplementationFileDeclaration.Entity(entity, subDecls) ->
                            yield! getExpressions subDecls
                        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(_,_,body) -> 
                            yield body
                        | _ -> () 
                }
                    
            getExpressions implementationFile.Declarations
        | None -> Seq.empty

    expressions
    |> Seq.choose (tryFindTypedExpression range)
    |> Seq.tryHead

let private matchesBuiltinFSharpType (typeName: string) (fsharpType: FSharpType) : Option<bool> =
    let matchingPartialInstanceMember =
        List.tryFind (fun (memberName: string, _, _, _) -> memberName.Split('.').[0] = typeName) partialInstanceMemberIdentifiers
    
    match matchingPartialInstanceMember with
    | Some(_, typeNamespace, compiledTypeName, _) ->
        Some(
            fsharpType.HasTypeDefinition
            && fsharpType.TypeDefinition.Namespace = typeNamespace
            && fsharpType.TypeDefinition.CompiledName = compiledTypeName
        )
    | None -> None

let private isNonStaticInstanceMemberCall (checkFile:FSharpCheckFileResults) names lineText (range: Range) :(Option<WarningDetails>) =
    let typeChecks =
        let map (replacement: string * option<string> * string * Replacement) =
            match replacement with
            | (fullyQualifiedInstanceMember, _, _, replacementStrategy) ->
                if not (fullyQualifiedInstanceMember.Contains ".") then
                    failwith "Please use fully qualified name for the instance member"
                let nameSegments = fullyQualifiedInstanceMember.Split '.'
                let instanceMemberNameOnly = Array.last nameSegments
                let isSourcePropSameAsReplacementProp = List.tryFind (fun sourceInstanceMemberName -> sourceInstanceMemberName = instanceMemberNameOnly) names
                match isSourcePropSameAsReplacementProp with
                | Some _ ->
                    let typeName = fullyQualifiedInstanceMember.Substring(0, fullyQualifiedInstanceMember.Length - instanceMemberNameOnly.Length - 1)                    

                    let instanceIdentifier = 
                        String.concat
                            "."
                            (List.takeWhile 
                                (fun sourceInstanceMemberName -> sourceInstanceMemberName <> instanceMemberNameOnly) 
                                names)
                    
                    let instanceIdentifierSymbol = 
                        let maybeSymbolUse = 
                            checkFile.GetSymbolUseAtLocation(
                                range.EndLine,
                                range.EndColumn - ((String.concat "." names).Length - instanceIdentifier.Length),
                                lineText,
                                List.singleton instanceIdentifier)
                        match maybeSymbolUse with
                        | Some symbolUse ->
                            match symbolUse.Symbol with
                            | :? FSharpMemberOrFunctionOrValue as symbol -> Some symbol
                            | _ -> None
                        | _ -> None
                    
                    match instanceIdentifierSymbol with
                    | Some identifierSymbol ->
                        let typeMatches =
                             let fsharpType = identifierSymbol.FullType
                             match matchesBuiltinFSharpType typeName fsharpType with
                             | Some value -> value
                             | None -> identifierSymbol.FullType.TypeDefinition.FullName = typeName

                        if typeMatches then
                            match replacementStrategy with
                            | PatternMatch ->
                                Some
                                    {
                                        Range = range
                                        Message =
                                            String.Format(
                                                Resources.GetString
                                                    "RulesConventionsNoPartialFunctionsPatternMatchError",
                                                fullyQualifiedInstanceMember
                                            )
                                        SuggestedFix = None
                                        TypeChecks = (fun () -> typeMatches) |> List.singleton
                                    }
                            | Function replacementFunctionName ->
                                Some
                                    {
                                        Range = range
                                        Message =
                                            String.Format(
                                                Resources.GetString "RulesConventionsNoPartialFunctionsReplacementError",
                                                replacementFunctionName,
                                                fullyQualifiedInstanceMember
                                            )
                                        SuggestedFix =
                                            Some(
                                                lazy
                                                    (Some
                                                        {
                                                            FromText = (String.concat "." names)
                                                            FromRange = range
                                                            ToText = replacementFunctionName
                                                        })
                                            )
                                        TypeChecks = (fun () -> typeMatches) |> List.singleton
                                    }
                        else
                            None
                    | _ -> None
                | _ -> None

        List.map map partialInstanceMemberIdentifiers
    match List.tryFind(fun (typeCheck:Option<WarningDetails>) -> typeCheck.IsSome) typeChecks with
    | None -> None
    | Some instanceMember -> instanceMember

let private checkMemberCallOnExpression 
    (checkFile: FSharpCheckFileResults) 
    (flieContent: string) 
    (range: Range) 
    (originalRange: Range): array<WarningDetails> =
    match getTypedExpressionForRange checkFile range with
    | Some expression ->
        let choose (fullyQualifiedInstanceMember: string) (replacementStrategy: Replacement) =
            let typeName = fullyQualifiedInstanceMember.Split(".").[0]
            let fsharpType = expression.Type

            let matchesType =
                match matchesBuiltinFSharpType typeName fsharpType with
                | Some value -> value
                | None -> 
                    fsharpType.HasTypeDefinition
                    && fsharpType.TypeDefinition.FullName = typeName

            if matchesType then
                match replacementStrategy with
                | PatternMatch ->
                    Some
                        {
                            Range = originalRange
                            Message =
                                String.Format(
                                    Resources.GetString "RulesConventionsNoPartialFunctionsPatternMatchError",
                                    fullyQualifiedInstanceMember
                                )
                            SuggestedFix = None
                            TypeChecks = (fun () -> true) |> List.singleton
                        }
                | Function replacementFunctionName ->
                    Some
                        {
                            Range = originalRange
                            Message =
                                String.Format(
                                    Resources.GetString "RulesConventionsNoPartialFunctionsReplacementError",
                                    replacementFunctionName,
                                    fullyQualifiedInstanceMember
                                )
                            SuggestedFix =
                                Some(
                                    lazy
                                        (Some
                                            {
                                                FromText =
                                                    (ExpressionUtilities.tryFindTextOfRange originalRange flieContent)
                                                        .Value
                                                FromRange = originalRange
                                                ToText = replacementFunctionName
                                            })
                                )
                            TypeChecks = (fun () -> true) |> List.singleton
                        }
            else
                None

        partialInstanceMemberIdentifiers
        |> List.choose (fun (fullyQualifiedInstanceMember, _, _, replacementStrategy) ->
            choose fullyQualifiedInstanceMember replacementStrategy)
        |> List.toArray
    | None -> Array.empty

let private runner (config:Config) (args:AstNodeRuleParams) =
    match (args.AstNode, args.CheckInfo) with
    | (AstNode.Identifier (identifier, range), Some checkInfo) ->
        let checkPartialIdentifier =
            checkIfPartialIdentifier config (String.concat "." identifier) range

        match checkPartialIdentifier with
        | Some partialIdent ->
            Array.singleton partialIdent
        | _ ->
            let lineText = args.Lines.[range.EndLine - 1]
            let nonStaticInstanceMemberTypeCheckResult = isNonStaticInstanceMemberCall checkInfo identifier lineText range
            match nonStaticInstanceMemberTypeCheckResult with
            | Some warningDetails ->
                Array.singleton warningDetails
            | _ -> Array.Empty()
    | (Ast.Expression(SynExpr.DotGet(expr, _, SynLongIdent(_identifiers, _, _), _range)), Some checkInfo) ->
        let originalRange = expr.Range
        let expr = ExpressionUtilities.removeParens expr

        checkMemberCallOnExpression checkInfo args.FileContent expr.Range originalRange
    | _ -> Array.empty

let rule config =
    AstNodeRule
        {
            Name = "NoPartialFunctions"
            Identifier = Identifiers.NoPartialFunctions
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
