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

let runner (config: Config) args =
    let symbol = config.Symbol
    match args.AstNode with
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, _, pattern, _, _, range, _)) ->
        match pattern with
        | SynPat.LongIdent(LongIdentWithDots(identifiers, _),_, _, _, _, _) ->
            match identifiers with
            | head::_ when head.idText <> config.Symbol -> 
               let error =
                   { Range = range
                     Message = String.Format(Resources.GetString "RulesFavourConsistentThis", config.Symbol)
                     SuggestedFix = None
                     TypeChecks = List.Empty }
                   |> Array.singleton
               error
            | _ -> Array.empty
        | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "FavourConsistentThis"
      Identifier = Identifiers.FavourConsistentThis
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule
