module FSharpLint.Rules.FavourConsistentThis

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System

/// Configuration of the favour consistent 'this' (FL0074) rule.
[<RequireQualifiedAccess>]
type Config = { Symbol: string }

let internal isNotConsistent identifier symbol =
    identifier <> symbol && identifier <> "_" && identifier <> "__"

let runner (config: Config) args =
    let symbol = config.Symbol
    match args.AstNode with
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, _, pattern, _, _, range, _, _)) ->
        match pattern with
        | SynPat.LongIdent(SynLongIdent(identifiers, _, _), _, _, _, _, _) ->
            if identifiers.Length = 2 then
                match identifiers  with
                | head::_ when isNotConsistent head.idText symbol ->
                    let fix = lazy(Some({ FromRange = head.idRange; ToText = symbol }))
                    let error =
                        Array.singleton
                            { Range = range
                              Message = String.Format(Resources.GetString "RulesFavourConsistentThis", config.Symbol)
                              Fix = Some fix
                              TypeChecks = List.Empty }
                    error
                | _ -> Array.empty
            else
                Array.empty
        | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    AstNodeRule
        {
            Name = "FavourConsistentThis"
            Identifier = Identifiers.FavourConsistentThis
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
