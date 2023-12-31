module FSharpLint.Rules.FavourNonMutablePropertyInitialization

open FSharpLint.Framework.Rules

let runner _args =
    failwith "Not implemented yet"

let rule =
    { Name = "FavourNonMutablePropertyInitialization"
      Identifier = Identifiers.FavourNonMutablePropertyInitialization
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
