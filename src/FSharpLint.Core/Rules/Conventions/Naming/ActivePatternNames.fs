module FSharpLint.Rules.ActivePatternNames

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getValueOrFunctionIdents isPublic pattern =
    match pattern with
    | SynPat.LongIdent(longIdent, _, _, _, _, _) ->
        // If a pattern identifier is made up of more than one part then it's not binding a new value.
        let singleIdentifier = List.length longIdent.Lid = 1

        match List.tryLast longIdent.Lid with
        | Some ident when isActivePattern ident ->
            ident |> Array.singleton
        | _ ->
            Array.empty
    | SynPat.Named(_, ident, _, _, _)
    | SynPat.OptionalVal(ident, _) ->
        if isActivePattern ident then
            ident |> Array.singleton
        else
            Array.empty
    | _ -> Array.empty

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(componentInfo, typeDef, _, _)) ->
        let isNotTypeExtension =
            match typeDef with
            | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconAugmentation, _, _) -> false
            | _ -> true

        if isNotTypeExtension then
            match componentInfo with
            | SynComponentInfo.ComponentInfo(attrs, _, _, identifier, _, _, _, _) ->
                match List.tryLast identifier with
                | Some typeIdentifier ->
                    if isMeasureType attrs then
                        typeIdentifier |> Array.singleton
                    else
                        Array.empty
                | _ -> Array.empty
        else
            Array.empty
    | AstNode.Expression(SynExpr.ForEach(_, _, true, pattern, _, _, _)) ->
        getPatternIdents false getValueOrFunctionIdents false pattern
    | AstNode.Binding(SynBinding.Binding(access, _, _, _, attributes, _, valData, pattern, _, _, _, _)) ->
        if not (isLiteral attributes) then
            match identifierTypeFromValData valData with
            | Value | Function ->
                let isPublic = isPublic args.SyntaxArray args.SkipArray args.NodeIndex
                getPatternIdents isPublic getValueOrFunctionIdents true pattern
            | _ -> Array.empty
        else
            Array.empty
    | _ -> Array.empty

let private getIdentifiersWithIdText =
    getIdentifiers
    >> Array.collect (fun identifier ->
        activePatternIdentifiers identifier
        |> Array.map (fun idText -> (identifier, idText, None)))

let rule config =
    { Name = "ActivePatternNames"
      Identifier = Identifiers.ActivePatternNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiersWithIdText } }
    |> toAstNodeRule
    |> AstNodeRule