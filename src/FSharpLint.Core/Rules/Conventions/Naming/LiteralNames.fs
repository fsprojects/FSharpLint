module FSharpLint.Rules.LiteralNames

open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding(SynBinding.Binding(_, _, _, _, attributes, _, _, pattern, _, _, _, _)) ->
        if isLiteral attributes then
            let rec getLiteralIdents = function
                | SynPat.Named(_, identifier, _, _, _) ->
                    (identifier, identifier.idText, None) |> Array.singleton
                | SynPat.Paren(p, _) -> getLiteralIdents p
                | _ -> Array.empty

            getLiteralIdents pattern
        else
            Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "LiteralNames"
      Identifier = Identifiers.LiteralNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule