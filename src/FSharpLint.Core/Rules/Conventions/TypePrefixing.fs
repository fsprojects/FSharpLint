module FSharpLint.Rules.TypePrefixing

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.ExpressionUtilities

type Mode =
    | Hybrid = 0
    | Always = 1
    | Never = 2

[<RequireQualifiedAccess>]
type Config = { Mode: Mode }

type CheckTypePrefixingConfig =
    {
        Config: Config
        Args: AstNodeRuleParams
        Range: FSharp.Compiler.Text.Range
        TypeName: SynType
        TypeArgs: string option
        IsPostfix: bool
    }

let checkTypePrefixing (typePrefixingConfig: CheckTypePrefixingConfig) =
    let recommendPostfixViolationMsg = lazy(Resources.GetString "RulesFormattingF#PostfixGenericViolation")
    match typePrefixingConfig.TypeName with
    | SynType.LongIdent lid ->
        let prefixSuggestion typeName =
            let suggestedFix = lazy(
                (ExpressionUtilities.tryFindTextOfRange typePrefixingConfig.Range typePrefixingConfig.Args.FileContent, typePrefixingConfig.TypeArgs)
                ||> Option.map2 (fun fromText typeArgs -> { FromText = fromText; FromRange = typePrefixingConfig.Range; ToText = $"{typeName}<{typeArgs}>" }))
            Some
                {
                    Range = typePrefixingConfig.Range
                    Message = Resources.GetString "RulesFormattingGenericPrefixViolation"
                    SuggestedFix = Some suggestedFix
                    TypeChecks = List.Empty
                }

        match longIdentWithDotsToString lid with
        | "list"
        | "List"
        | "option"
        | "Option"
        | "ref"
        | "Ref" as typeName ->

            // Prefer postfix.
            if not typePrefixingConfig.IsPostfix && typePrefixingConfig.Config.Mode <> Mode.Always
            then
                let suggestedFix = lazy(
                    (ExpressionUtilities.tryFindTextOfRange typePrefixingConfig.Range typePrefixingConfig.Args.FileContent, typePrefixingConfig.TypeArgs)
                    ||> Option.map2 (fun fromText typeArgs -> { FromText = fromText; FromRange = typePrefixingConfig.Range; ToText = $"{typeArgs} {typeName}" }))
                Some
                    {
                        Range = typePrefixingConfig.Range
                        Message = String.Format(recommendPostfixViolationMsg.Value, typeName)
                        SuggestedFix = Some suggestedFix
                        TypeChecks = List.Empty
                    }
            else
                if typePrefixingConfig.IsPostfix && typePrefixingConfig.Config.Mode = Mode.Always then
                    prefixSuggestion typeName
                else
                    None

        | "array" when typePrefixingConfig.Config.Mode <> Mode.Always ->
            // Prefer special postfix (e.g. int []).
            let suggestedFix = lazy(
                (ExpressionUtilities.tryFindTextOfRange typePrefixingConfig.Range typePrefixingConfig.Args.FileContent, typePrefixingConfig.TypeArgs)
                ||> Option.map2 (fun fromText typeArgs -> { FromText = fromText; FromRange = typePrefixingConfig.Range; ToText = $"{typeArgs} []" }))
            Some
                {
                    Range = typePrefixingConfig.Range
                    Message = Resources.GetString "RulesFormattingF#ArrayPostfixViolation"
                    SuggestedFix = Some suggestedFix
                    TypeChecks = List.Empty
                }

        | typeName ->
            match (typePrefixingConfig.IsPostfix, typePrefixingConfig.Config.Mode) with
            | true, Mode.Never ->
                None
            | true, _ ->
                prefixSuggestion typeName
            | false, Mode.Never ->
                Some
                    {
                        Range = typePrefixingConfig.Range
                        Message = String.Format(recommendPostfixViolationMsg.Value, typeName)
                        // TODO
                        SuggestedFix = None
                        TypeChecks = List.Empty
                    }
            | false, _ ->
                None
    | _ ->
        None

let runner (config:Config) args =
    match args.AstNode with
    | AstNode.Type (SynType.App (typeName, _, typeArgs, _, _, isPostfix, range)) ->
        let typeArgs = typeArgsToString args.FileContent typeArgs
        checkTypePrefixing
            {
                Config = config
                Args = args
                Range = range
                TypeName = typeName
                TypeArgs = typeArgs
                IsPostfix = isPostfix
            }
        |> Option.toArray
    | AstNode.Type (SynType.Array (1, _elementType, range)) when config.Mode = Mode.Always ->
        Array.singleton
            { Range = range
              Message = Resources.GetString "RulesFormattingF#ArrayPrefixViolation"
              SuggestedFix = None
              TypeChecks = List.Empty }
    | _ ->
        Array.empty


let rule config =
    AstNodeRule
        {
            Name = "TypePrefixing"
            Identifier = Identifiers.TypePrefixing
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
