module FSharpLint.Rules.InternalValuesNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let rule config =
    let getIdentifiers (args:AstNodeRuleParams) =
        let getValueOrFunctionIdents typeChecker accessControlLevel pattern =
            let checkNotUnionCase ident = fun () ->
                typeChecker
                |> Option.map (fun checker -> isNotUnionCase checker ident)
                |> Option.defaultValue false

            match pattern with
            | SynPat.LongIdent(longIdent, _, _, _, _, _) ->
                // If a pattern identifier is made up of more than one part then it's not binding a new value.
                let singleIdentifier = List.length longIdent.LongIdent = 1

                match List.tryLast longIdent.LongIdent with
                | Some ident when not (isActivePattern ident) && singleIdentifier ->
                    if accessControlLevel = AccessControlLevel.Internal then
                        Array.singleton (ident, ident.idText, Some (checkNotUnionCase ident)) 
                    else
                        Array.empty
                | None | Some _ -> Array.empty
            | _ -> Array.empty

        match args.AstNode with
        | AstNode.Binding(SynBinding(_access, _, _, _, attributes, _, valData, pattern, _, _, _, _, _)) ->
            if not (isLiteral attributes) then
                match identifierTypeFromValData valData with
                | Value | Function ->
                    let accessibility = getAccessControlLevel args.SyntaxArray args.NodeIndex
                    getPatternIdents accessibility (getValueOrFunctionIdents args.CheckInfo) true pattern
                | _ -> Array.empty
            else
                Array.empty
        | _ -> Array.empty

    { Name = "InternalValuesNames"
      Identifier = Identifiers.InternalValuesNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule
