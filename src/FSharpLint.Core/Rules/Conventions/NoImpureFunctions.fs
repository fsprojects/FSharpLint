module FSharpLint.Rules.NoImpureFunctions

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax

[<RequireQualifiedAccess>]
type Config = {
    AllowedImpureFunctions:string list
    AdditionalImpureFunctions:string list
}

let private impureFunctionIdentifiers =
    Map.ofList
        [
            ("Array.sortInPlace", Some "Array.sort")
            ("Array.sortInPlaceBy", Some "Array.Array.sortBy")
            ("Array.sortInPlaceWith", Some "Array.sortWith")

            ("Array.set", Some "Array.updateAt")

            ("Array.fill", None)
            ("Array.blit", None)

            ("Array2D.fill", None)
            ("Array2D.blit", None)

            // Disallowed types
            ("System.Collections.Generic.List.*", None)
            ("System.Collections.Generic.Dictionary.*", None)
            ("System.Collections.Generic.HashSet.*", None)
            ("System.Collections.Generic.LinkedList.*", None)
            ("System.Collections.Generic.SortedDictionary.*", None)
            ("System.Collections.Generic.SortedSet.*", None)
            ("System.Collections.Generic.Stack.*", None)
        ]

let runner (config:Config) (args:AstNodeRuleParams) =
    let checkIfImpureIdentifier (identifier:string) (range:Range) =
        let issueWarning maybeReplacementFunction =
            match maybeReplacementFunction with
            | Some replacementFunction ->
                {
                    Range = range
                    Message = String.Format(Resources.GetString "RulesConventionsNoImpureFunctionsReplacementError", replacementFunction, identifier)
                    SuggestedFix = Some (lazy ( Some { FromText = identifier; FromRange = range; ToText = replacementFunction }))
                    TypeChecks = List.Empty
                }
            | None -> 
                {
                    Range = range
                    Message = String.Format(Resources.GetString "RulesConventionsNoImpureFunctionsError", identifier)
                    SuggestedFix = None
                    TypeChecks = List.Empty
                }

        if List.contains identifier config.AllowedImpureFunctions then
            None
        elif List.contains identifier config.AdditionalImpureFunctions then
            issueWarning None |> Some
        else
            Map.tryFind identifier impureFunctionIdentifiers
            |> Option.filter (fun _ -> not (List.contains identifier config.AllowedImpureFunctions))
            |> Option.map issueWarning
    
    let disallowedTypes =
        let builtinIdentifiers = Map.keys impureFunctionIdentifiers
        (Seq.append config.AdditionalImpureFunctions builtinIdentifiers)
        |> Seq.choose (fun definition ->
            let wildcardSuffix = ".*"
            if definition.EndsWith wildcardSuffix then
                Some <| definition.Substring(0, definition.Length - wildcardSuffix.Length)
            else
                None)
        |> Seq.toList

    let checkForDisallowedType (checkResults: FSharpCheckFileResults) (range: Range) =
        let allSymbolUses = checkResults.GetAllUsesOfAllSymbolsInFile()
        let maybeSymbolUse =
            allSymbolUses
            |> Seq.tryFind (fun symbolUse -> range = symbolUse.Range)
        match maybeSymbolUse with
        | Some symbolUse ->
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as value when value.IsConstructor || value.IsImplicitConstructor ->
                let declaringEntity = value.DeclaringEntity
                match declaringEntity with
                | Some entity ->
                    let fullNameWithoutTypeParams =
                        let lestBackTickIndex = entity.FullName.LastIndexOf '`'
                        if lestBackTickIndex > 0 then
                            entity.FullName.Substring(0, lestBackTickIndex)
                        else
                            entity.FullName
                        
                    if disallowedTypes |> List.contains fullNameWithoutTypeParams then
                        Array.singleton
                            {
                                Range = range
                                Message = String.Format(Resources.GetString "RulesConventionsNoImpureFunctionsDisallowedTypeError", entity.FullName)
                                SuggestedFix = None
                                TypeChecks = List.Empty
                            }
                    else
                        Array.empty
                | _ -> Array.empty
            | _ -> Array.empty
        | None -> Array.empty

    match (args.AstNode, args.CheckInfo) with
    | AstNode.Identifier (identifier, range), _ ->
        match checkIfImpureIdentifier (String.concat "." identifier) range with
        | Some impureIdentifierWarning ->
            Array.singleton impureIdentifierWarning
        | _ ->
            Array.Empty()
    | AstNode.Expression(SynExpr.App(_, false, funcExpr, _, _range)), Some(checkResults) when not disallowedTypes.IsEmpty ->
        match funcExpr with
        | SynExpr.LongIdent(_, SynLongIdent(_), _, identRange) ->
            checkForDisallowedType checkResults identRange
        | SynExpr.TypeApp(SynExpr.LongIdent(_, SynLongIdent(_), _, identRange),_,_,_,_,_,_) -> 
            checkForDisallowedType checkResults identRange
        | _ -> Array.empty
    | AstNode.Expression(SynExpr.New(_, targetType, _, _)), Some(checkResults) when not disallowedTypes.IsEmpty ->
        checkForDisallowedType checkResults targetType.Range
    | _ -> Array.empty

let rule config =
    AstNodeRule
        {
            Name = "NoImpureFunctions"
            Identifier = Identifiers.NoImpureFunctions
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
