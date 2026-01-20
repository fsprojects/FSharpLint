module FSharpLint.Rules.LiteralNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let rule config =
    let getIdentifiers (args:AstNodeRuleParams) =
        match args.AstNode with
        | AstNode.Binding(SynBinding(_, _, _, _, attributes, _, _, pattern, _, _, _, _, _)) ->
            if isLiteral attributes then
                let rec getLiteralIdents = function
                    | SynPat.Named(SynIdent(identifier, _), _, _, _) ->
                        Array.singleton (identifier, identifier.idText, None)
                    | SynPat.LongIdent(SynLongIdent([identifier], _, _), _, _, _, _, _) ->
                        Array.singleton (identifier, identifier.idText, None)
                    | SynPat.Paren(pat, _) -> getLiteralIdents pat
                    | _ -> Array.empty

                getLiteralIdents pattern
            else
                Array.empty
        | _ -> Array.empty

    { Name = "LiteralNames"
      Identifier = Identifiers.LiteralNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule