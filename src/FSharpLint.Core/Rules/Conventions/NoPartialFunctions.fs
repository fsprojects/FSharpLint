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

// There is no need to recreate the map on every function call.
//fsharplint:disable FavourLocalOverPrivate
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
//fsharplint:disable FavourLocalOverPrivate

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

[<TailCall>]
let rec private tryFindTypedExpression (range: Range) (expressions: List<FSharpExpr>): Option<FSharpExpr> = 
    match expressions with
    | expression :: _ when expression.Range = range ->
        Some expression
    | FSharpExprPatterns.AddressOf(lvalueExpr) :: rest -> 
        tryFindTypedExpression range (lvalueExpr :: rest)
    | FSharpExprPatterns.AddressSet(lvalueExpr, rvalueExpr) :: rest -> 
        tryFindTypedExpression range (lvalueExpr :: rvalueExpr :: rest)
    | FSharpExprPatterns.Application(funcExpr, _typeArgs, argExprs) :: rest -> 
        tryFindTypedExpression range (funcExpr :: argExprs @ rest)
    | FSharpExprPatterns.Call(objExprOpt, _memberOrFunc, _typeArgs1, _typeArgs2, argExprs) :: rest ->
        tryFindTypedExpression range ((Option.toList objExprOpt) @ argExprs @ rest)
    | FSharpExprPatterns.Coerce(_targetType, inpExpr) :: rest -> 
        tryFindTypedExpression range (inpExpr :: rest)
    | FSharpExprPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, _isUp, _, _) :: rest -> 
        tryFindTypedExpression range ([ startExpr; limitExpr; consumeExpr ] @ rest)
    | FSharpExprPatterns.ILAsm(_asmCode, _typeArgs, argExprs) :: rest -> 
        tryFindTypedExpression range (argExprs @ rest)
    | FSharpExprPatterns.ILFieldGet (objExprOpt, _fieldType, _fieldName) :: rest -> 
        tryFindTypedExpression range (Option.toList objExprOpt @ rest)
    | FSharpExprPatterns.ILFieldSet (objExprOpt, _fieldType, _fieldName, valueExpr) :: rest -> 
        tryFindTypedExpression range (Option.toList objExprOpt @ valueExpr :: rest)
    | FSharpExprPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) :: rest -> 
        tryFindTypedExpression range ([ guardExpr; thenExpr; elseExpr ] @ rest)
    | FSharpExprPatterns.Lambda(_lambdaVar, bodyExpr) :: rest -> 
        tryFindTypedExpression range (bodyExpr :: rest)
    | FSharpExprPatterns.Let((_bindingVar, bindingExpr, _), bodyExpr) :: rest -> 
        tryFindTypedExpression range (bindingExpr :: bodyExpr :: rest)
    | FSharpExprPatterns.LetRec(recursiveBindings, bodyExpr) :: rest ->
        let bindingExprs =
            recursiveBindings 
            |> List.map (fun (_, expr, _) -> expr)
        tryFindTypedExpression range (bindingExprs @ bodyExpr :: rest)
    | FSharpExprPatterns.NewArray(_arrayType, argExprs) :: rest -> 
        tryFindTypedExpression range (argExprs @ rest)
    | FSharpExprPatterns.NewDelegate(_delegateType, delegateBodyExpr) :: rest -> 
        tryFindTypedExpression range (delegateBodyExpr :: rest)
    | FSharpExprPatterns.NewObject(_objType, _typeArgs, argExprs) :: rest -> 
        tryFindTypedExpression range (argExprs @ rest)
    | FSharpExprPatterns.NewRecord(_recordType, argExprs) :: rest ->  
        tryFindTypedExpression range (argExprs @ rest)
    | FSharpExprPatterns.NewAnonRecord(_recordType, argExprs) :: rest ->  
        tryFindTypedExpression range (argExprs @ rest)
    | FSharpExprPatterns.NewTuple(_tupleType, argExprs) :: rest -> 
        tryFindTypedExpression range (argExprs @ rest)
    | FSharpExprPatterns.NewUnionCase(_unionType, _unionCase, argExprs) :: rest -> 
        tryFindTypedExpression range (argExprs @ rest)
    | FSharpExprPatterns.Quote(quotedExpr) :: rest -> 
        tryFindTypedExpression range (quotedExpr :: rest)
    | FSharpExprPatterns.FSharpFieldGet(objExprOpt, _recordOrClassType, _fieldInfo) :: rest -> 
         tryFindTypedExpression range (Option.toList objExprOpt @ rest)
    | FSharpExprPatterns.AnonRecordGet(objExpr, _recordOrClassType, _fieldInfo) :: rest -> 
        tryFindTypedExpression range (objExpr :: rest)
    | FSharpExprPatterns.FSharpFieldSet(objExprOpt, _recordOrClassType, _fieldInfo, argExpr) :: rest -> 
        tryFindTypedExpression range (Option.toList objExprOpt @ argExpr :: rest)
    | FSharpExprPatterns.Sequential(firstExpr, secondExpr) :: rest -> 
        tryFindTypedExpression range (firstExpr :: secondExpr :: rest)
    | FSharpExprPatterns.TryFinally(bodyExpr, finalizeExpr, _, _) :: rest -> 
        tryFindTypedExpression range (bodyExpr :: finalizeExpr :: rest)
    | FSharpExprPatterns.TryWith(bodyExpr, _, _, _catchVar, catchExpr, _, _) :: rest -> 
        tryFindTypedExpression range (bodyExpr :: catchExpr :: rest)
    | FSharpExprPatterns.TupleGet(_tupleType, _tupleElemIndex, tupleExpr) :: rest -> 
        tryFindTypedExpression range (tupleExpr :: rest)
    | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) :: rest -> 
        let decisionTargetExprs =
            decisionTargets |> List.map (fun (_, expr) -> expr)
        tryFindTypedExpression range (decisionExpr :: decisionTargetExprs @ rest)
    | FSharpExprPatterns.DecisionTreeSuccess (_decisionTargetIdx, decisionTargetExprs) :: rest -> 
        tryFindTypedExpression range (decisionTargetExprs @ rest)
    | FSharpExprPatterns.TypeLambda(_genericParam, bodyExpr) :: rest -> 
        tryFindTypedExpression range (bodyExpr :: rest)
    | FSharpExprPatterns.TypeTest(_ty, inpExpr) :: rest -> 
        tryFindTypedExpression range (inpExpr :: rest)
    | FSharpExprPatterns.UnionCaseSet(unionExpr, _unionType, _unionCase, _unionCaseField, valueExpr) :: rest -> 
        tryFindTypedExpression range (unionExpr :: valueExpr :: rest)
    | FSharpExprPatterns.UnionCaseGet(unionExpr, _unionType, _unionCase, _unionCaseField) :: rest -> 
        tryFindTypedExpression range (unionExpr :: rest)
    | FSharpExprPatterns.UnionCaseTest(unionExpr, _unionType, _unionCase) :: rest -> 
        tryFindTypedExpression range (unionExpr :: rest)
    | FSharpExprPatterns.UnionCaseTag(unionExpr, _unionType) :: rest -> 
        tryFindTypedExpression range (unionExpr :: rest)
    | FSharpExprPatterns.ObjectExpr(_objType, baseCallExpr, overrides, interfaceImplementations) :: rest -> 
        let interfaceImlps = 
            interfaceImplementations
            |> List.collect (fun (_, imlps) -> imlps)
        let exprs =
            List.append overrides interfaceImlps
            |> Seq.cast<FSharpExpr>
            |> Seq.toList
        tryFindTypedExpression range (baseCallExpr :: exprs @ rest)
    | FSharpExprPatterns.TraitCall(_sourceTypes, _traitName, _typeArgs, _typeInstantiation, _argTypes, argExprs) :: rest -> 
        tryFindTypedExpression range (argExprs @ rest)
    | FSharpExprPatterns.ValueSet(_valToSet, valueExpr) :: rest -> 
        tryFindTypedExpression range (valueExpr :: rest)
    | FSharpExprPatterns.WhileLoop(guardExpr, bodyExpr, _) :: rest -> 
        tryFindTypedExpression range (guardExpr :: bodyExpr :: rest)
    | _ :: rest ->
        tryFindTypedExpression range rest
    | [] -> None

[<TailCall>]
let rec private getExpressions acc declarations =
    match declarations with
    | [] -> acc
    | declaration :: rest ->
        match declaration with
        | FSharpImplementationFileDeclaration.Entity(_entity, subDecls) ->
            getExpressions acc (subDecls @ rest)
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(_,_,body) -> 
            getExpressions (body :: acc) rest
        | _ -> 
            getExpressions acc rest

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

let checkMemberCallOnExpression 
    (checkFile: FSharpCheckFileResults) 
    (flieContent: string) 
    (range: Range) 
    (originalRange: Range): array<WarningDetails> =
    let typedExpression =
        let expressions =
            match checkFile.ImplementationFile with
            | Some implementationFile -> getExpressions List.empty implementationFile.Declarations
            | None -> List.empty

        tryFindTypedExpression range expressions

    match typedExpression with
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

let runner (config:Config) (args:AstNodeRuleParams) =
    let checkIfPartialIdentifier (identifier:string) (range:Range) =
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

    let isNonStaticInstanceMemberCall (checkFile:FSharpCheckFileResults) names lineText (range: Range) :(Option<WarningDetails>) =
        let typeChecks =
            let map (replacement: string * option<string> * string * Replacement) =
                match replacement with
                | (fullyQualifiedInstanceMember, _, _, replacementStrategy) ->
                    if not (fullyQualifiedInstanceMember.Contains ".") then
                        failwith "Please use fully qualified name for the instance member"
                    let nameSegments = fullyQualifiedInstanceMember.Split '.'
                    let instanceMemberNameOnly =
                        Array.tryLast nameSegments
                        |> Option.defaultWith (fun () -> failwith $"{nameof(nameSegments)} is empty")
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

    match (args.AstNode, args.CheckInfo) with
    | (AstNode.Identifier (identifier, range), Some checkInfo) ->
        let checkPartialIdentifier =
            checkIfPartialIdentifier (String.concat "." identifier) range

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
