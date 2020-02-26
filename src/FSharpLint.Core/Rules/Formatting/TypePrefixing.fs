module FSharpLint.Rules.TypePrefixing

open System
open FSharp.Compiler.Ast
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.ExpressionUtilities

let checkTypePrefixing (args:AstNodeRuleParams) range typeName typeArgs isPostfix =
    match typeName with
    | SynType.LongIdent lid ->
        match lid |> longIdentWithDotsToString with
        | "list"
        | "List"
        | "option"
        | "Option"
        | "ref"
        | "Ref" as typeName ->
            // Prefer postfix.
            if not isPostfix
            then
                let errorFormatString = Resources.GetString("RulesFormattingF#PostfixGenericError")
                let suggestedFix = lazy(
                    (ExpressionUtilities.tryFindTextOfRange range args.FileContent, typeArgs)
                    ||> Option.map2 (fun fromText typeArgs -> { FromText = fromText; FromRange = range; ToText = typeArgs + " " + typeName }))
                { Range = range
                  Message =  String.Format(errorFormatString, typeName)
                  SuggestedFix = Some suggestedFix
                  TypeChecks = [] } |> Some
            else
                None
        | "array" ->
            // Prefer special postfix (e.g. int []).
            let suggestedFix = lazy(
                (ExpressionUtilities.tryFindTextOfRange range args.FileContent, typeArgs)
                ||> Option.map2 (fun fromText typeArgs -> { FromText = fromText; FromRange = range; ToText = typeArgs + " []" }))
            { Range = range
              Message = Resources.GetString("RulesFormattingF#ArrayPostfixError")
              SuggestedFix = Some suggestedFix
              TypeChecks = [] } |> Some
        | typeName ->
            // Prefer prefix.
            if isPostfix then
                let suggestedFix = lazy(
                    (ExpressionUtilities.tryFindTextOfRange range args.FileContent, typeArgs)
                    ||> Option.map2 (fun fromText typeArgs -> { FromText = fromText; FromRange = range; ToText = typeName + "<" + typeArgs + ">" }))
                { Range = range
                  Message = Resources.GetString("RulesFormattingGenericPrefixError")
                  SuggestedFix = Some suggestedFix
                  TypeChecks = [] } |> Some
            else
                None
    | _ ->
        None

let runner args =
    match args.AstNode with
    | AstNode.Type (SynType.App (typeName, _, typeArgs, _, _, isPostfix, range)) ->
        let typeArgs = typeArgsToString args.FileContent typeArgs
        checkTypePrefixing args range typeName typeArgs isPostfix
        |> Option.toArray
    | _ ->
        Array.empty


let rule =
    { Name = "TypePrefixing"
      Identifier = Identifiers.TypePrefixing
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
