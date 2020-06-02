module internal FSharpLint.Rules.RecordFieldNames

open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Field(SynField.Field(_, _, identifier, _, _, _, _, _)) ->
        identifier
        |> Option.map (fun identifier -> (identifier, identifier.idText, None))
        |> Option.toArray
    | _ -> Array.empty

let rule config =
    { Name = "RecordFieldNames"
      Identifier = Identifiers.RecordFieldNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule