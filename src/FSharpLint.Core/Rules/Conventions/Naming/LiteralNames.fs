module FSharpLint.Rules.LiteralNames

open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.Binding(SynBinding.Binding(access, _, _, _, attributes, _, valData, pattern, _, _, _, _)) ->
        if isLiteral attributes then
            let rec getLiteralIdents = function
            | SynPat.Named(_, identifier, _, _, _) ->
                identifier |> Array.singleton
            | SynPat.Paren(p, _) -> getLiteralIdents p
            | _ -> Array.empty

            getLiteralIdents pattern
        else
            Array.empty
    | _ -> Array.empty

let rule config =
    { name = "LiteralNames"
      identifier = Identifiers.LiteralNames
      ruleConfig = { NamingRuleConfig.config = config; getIdentifiersToCheck = getIdentifiers >> addDefaults } }
    |> toAstNodeRule
    |> AstNodeRule