module FSharpLint.Rules.LiteralNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding(SynBinding(_, _, _, _, attributes, _, _, pattern, _, _, _, _, _)) ->
        if isLiteral attributes then
            let rec getLiteralIdents = function
                | SynPat.Named(SynIdent(identifier, _), _, _, _) ->
                    (identifier, identifier.idText, None) |> Array.singleton
                | SynPat.Paren(pat, _) -> getLiteralIdents pat
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