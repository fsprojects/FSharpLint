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

[<RequireQualifiedAccess>]
type Config = {
    AllowedPartials:string list
    AdditionalPartials:string list
}

type private Replacement =
    | PatternMatch
    | Function of functionName:string

let private partialFunctionIdentifiers =
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
    ] |> Map.ofList

let private partialInstanceMemberIdentifiers =
    [
        ("Option.Value", PatternMatch)
        ("Map.Item", Function "Map.tryFind")
        ("List.Item", Function "List.tryFind")
        ("List.Head", Function "List.tryHead")

        // As an example for future additions (see commented Foo.Bar.Baz tests)
        //("Foo.Bar.Baz", PatternMatch)
    ] |> Map.ofList

let private checkIfPartialIdentifier (config:Config) (identifier:string) (range:Range) =
    if List.contains identifier config.AllowedPartials then
        None
    elif List.contains identifier config.AdditionalPartials then
        Some {
            Range = range
            Message = String.Format(Resources.GetString ("RulesConventionsNoPartialFunctionsAdditionalError"), identifier)
            SuggestedFix = None
            TypeChecks = []
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
                    TypeChecks = []
                }
            | Function replacementFunction ->
                {
                    Range = range
                    Message = String.Format(Resources.GetString "RulesConventionsNoPartialFunctionsReplacementError", replacementFunction, identifier)
                    SuggestedFix = Some (lazy ( Some { FromText = identifier; FromRange = range; ToText = replacementFunction }))
                    TypeChecks = []
                })

let private isNonStaticInstanceMemberCall (checkFile:FSharpCheckFileResults) names range:(Option<WarningDetails>) =

    let typeChecks =
        (partialInstanceMemberIdentifiers
        |> Map.toList
        |> List.map (fun replacement ->
            match replacement with
            | (fullyQualifiedInstanceMember, replacementStrategy) ->
                if not (fullyQualifiedInstanceMember.Contains ".") then
                    failwith "Please use fully qualified name for the instance member"
                let nameSegments = fullyQualifiedInstanceMember.Split '.'
                let instanceMemberNameOnly = Array.last nameSegments
                let isSourcePropSameAsReplacementProp = List.tryFind (fun sourceInstanceMemberName -> sourceInstanceMemberName = instanceMemberNameOnly) names
                match isSourcePropSameAsReplacementProp with
                | Some _ ->
                    let typeName = fullyQualifiedInstanceMember.Substring(0, fullyQualifiedInstanceMember.Length - instanceMemberNameOnly.Length - 1)
                    let partialAssemblySignature = checkFile.PartialAssemblySignature

                    let isEntityOfType (entity:FSharpEntity) =
                        match entity.TryFullName with
                        | Some name when name = typeName -> true
                        | _ -> false

                    let entityForType =
                        if partialAssemblySignature.Entities.Count > 1 then
                            Seq.tryFind isEntityOfType partialAssemblySignature.Entities
                        else
                            Some partialAssemblySignature.Entities.[0]

                    match entityForType with
                    | Some moduleEnt ->
                        let getFunctionValTypeName (fnVal:FSharpMemberOrFunctionOrValue) =
                             let fsharpType = fnVal.FullType
                             match typeName with
                             | "Option" ->
                                // see https://stackoverflow.com/a/70282499/544947
                                fsharpType.HasTypeDefinition
                                && fsharpType.TypeDefinition.Namespace = Some "Microsoft.FSharp.Core"
                                && fsharpType.TypeDefinition.CompiledName = "option`1"
                             | "Map" ->
                                fsharpType.HasTypeDefinition
                                && fsharpType.TypeDefinition.Namespace = Some "Microsoft.FSharp.Collections"
                                && fsharpType.TypeDefinition.CompiledName = "FSharpMap`2"
                             | "List" ->
                                fsharpType.HasTypeDefinition
                                && fsharpType.TypeDefinition.Namespace = Some "Microsoft.FSharp.Collections"
                                && fsharpType.TypeDefinition.CompiledName = "list`1"
                             | _ -> fnVal.FullName = typeName

                        let typeMatches = moduleEnt.MembersFunctionsAndValues.Any(Func<FSharpMemberOrFunctionOrValue, bool>(getFunctionValTypeName))
                        if typeMatches then
                            match replacementStrategy with
                             | PatternMatch ->
                                Some { Range = range
                                       Message = String.Format(Resources.GetString "RulesConventionsNoPartialFunctionsPatternMatchError", fullyQualifiedInstanceMember)
                                       SuggestedFix = None
                                       TypeChecks = (fun () -> typeMatches) |> List.singleton }
                             | Function replacementFunctionName ->
                                Some { Range = range
                                       Message = String.Format(Resources.GetString "RulesConventionsNoPartialFunctionsReplacementError", replacementFunctionName, fullyQualifiedInstanceMember)
                                       SuggestedFix = Some (lazy ( Some { FromText = (String.concat "." names) ; FromRange = range; ToText = replacementFunctionName }))
                                       TypeChecks = (fun () -> typeMatches) |> List.singleton }
                        else
                            None
                    | _ -> None
                | _ -> None))
    match List.tryFind(fun (typeCheck:Option<WarningDetails>) -> typeCheck.IsSome) typeChecks with
    | None -> None
    | Some instanceMember -> instanceMember

let private runner (config:Config) (args:AstNodeRuleParams) =
    match (args.AstNode, args.CheckInfo) with
    | (AstNode.Identifier (identifier, range), Some checkInfo) ->
        let checkPartialIdentifier =
            checkIfPartialIdentifier config (String.concat "." identifier) range

        match checkPartialIdentifier with
        | Some partialIdent ->
            partialIdent |> Array.singleton
        | _ ->
            let nonStaticInstanceMemberTypeCheckResult = isNonStaticInstanceMemberCall checkInfo identifier range
            match nonStaticInstanceMemberTypeCheckResult with
            | Some warningDetails ->
                warningDetails |> Array.singleton
            | _ -> Array.Empty()
    | _ -> Array.empty

let rule config =
    { Name = "NoPartialFunctions"
      Identifier = Identifiers.NoPartialFunctions
      RuleConfig = { AstNodeRuleConfig.Runner = runner config
                     Cleanup = ignore } }
    |> AstNodeRule
