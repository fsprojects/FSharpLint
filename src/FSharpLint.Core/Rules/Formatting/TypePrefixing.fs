module FSharpLint.Rules.TypePrefixing

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.ExpressionUtilities

type Mode =
    | Hybrid = 0
    | Always = 1
    | Never = 2

[<RequireQualifiedAccess>]
type Config = { Mode: Mode }

let checkTypePrefixing (config:Config) (args:AstNodeRuleParams) range typeName typeArgs isPostfix =
    match typeName with
    | SynType.LongIdent lid ->
        let prefixSuggestion typeName =
            let suggestedFix = lazy(
                (ExpressionUtilities.tryFindTextOfRange range args.FileContent, typeArgs)
                ||> Option.map2 (fun fromText typeArgs -> { FromText = fromText; FromRange = range; ToText = typeName + "<" + typeArgs + ">" }))
            { Range = range
              Message = Resources.GetString("RulesFormattingGenericPrefixError")
              SuggestedFix = Some suggestedFix
              TypeChecks = [] } |> Some

        match lid |> longIdentWithDotsToString with
        | "list"
        | "List"
        | "option"
        | "Option"
        | "ref"
        | "Ref" as typeName ->

            // Prefer postfix.
            if not isPostfix && config.Mode <> Mode.Always
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
                if isPostfix && config.Mode = Mode.Always then
                    prefixSuggestion typeName
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
                prefixSuggestion typeName
            else
                None
    | _ ->
        None

let runner (config:Config) args =
    match args.AstNode with
    | AstNode.Type (SynType.App (typeName, _, typeArgs, _, _, isPostfix, range)) ->
        let typeArgs = typeArgsToString args.FileContent typeArgs
        checkTypePrefixing config args range typeName typeArgs isPostfix
        |> Option.toArray
    | _ ->
        Array.empty


let rule config =
    { Name = "TypePrefixing"
      Identifier = Identifiers.TypePrefixing
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule
