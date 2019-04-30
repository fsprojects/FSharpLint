module FSharpLint.Rules.RecordFieldNames

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.Field(SynField.Field(_, _, identifier, _, _, _, _, _)) ->
        identifier |> Option.toArray
    | _ -> Array.empty

let rule config =
    { name = "RecordFieldNames" 
      identifier = Identifiers.RecordFieldNames
      ruleConfig = { NamingRuleConfig.config = config; getIdentifiersToCheck = getIdentifiers >> addDefaults } }
    |> toAstNodeRule
    |> AstNodeRule
