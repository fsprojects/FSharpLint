module FSharpLint.Rules.RecordFieldNames

open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.TypeSimpleRepresentation (SynTypeDefnSimpleRepr.Record (recordFields=recordFields)) ->
        recordFields
        |> List.choose (fun (SynField.Field (idOpt=idOpt)) ->
            idOpt
            |> Option.map (fun identifier -> (identifier, identifier.idText, None)))
        |> List.toArray
    | _ -> Array.empty

let rule config =
    { Name = "RecordFieldNames"
      Identifier = Identifiers.RecordFieldNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule