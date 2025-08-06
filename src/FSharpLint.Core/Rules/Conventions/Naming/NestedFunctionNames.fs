module FSharpLint.Rules.NestedFunctionNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding (SynBinding (_, _, _, _, _attributes, _, valData, pattern, _, _, _, _, _)) ->
        if isNested args args.NodeIndex then
            let maxAccessibility = AccessControlLevel.Public
            match identifierTypeFromValData valData with
            | Function | Member ->
                getPatternIdents maxAccessibility (fun _a11y innerPattern -> getFunctionIdents innerPattern) true pattern
            | _ -> Array.empty
        else
            Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "NestedFunctionNames"
      Identifier = Identifiers.NestedFunctionNames
      RuleConfig =
        { NamingRuleConfig.Config = config
          GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule
