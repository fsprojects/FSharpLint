module FSharpLint.Rules.InterpolatedStringWithNoSubstitution

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private makeWarning range =
    {
        Range = range
        Message = Resources.GetString "InterpolatedStringWithNoSubstitution"
        SuggestedFix = None
        TypeChecks = List.Empty
    }

let checkInterpolatedString (contents: List<SynInterpolatedStringPart>) range =
    if contents |> List.exists (fun part -> part.IsFillExpr) then
        Array.empty
    else
        Array.singleton (makeWarning range)

let stringFormattingFunctionNames = [ "sprintf"; "failwithf" ]

// https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/plaintext-formatting#format-specifiers-for-printf
let formatSpecifierRegex = Text.RegularExpressions.Regex(@"($|[^\%])\%[^\s\%]")

let checkFormattingFunctionApplication (formatString: string) range =
    if formatSpecifierRegex.IsMatch formatString then
        Array.empty
    else
        Array.singleton (makeWarning range)
            
let runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.InterpolatedString(contents, _synStringKind, range)) ->
        checkInterpolatedString contents range
    | AstNode.Expression(SynExpr.App(_, false, SynExpr.Ident(ident), SynExpr.Const(SynConst.String(text, _, _stringRange),_), range))
        when stringFormattingFunctionNames |> List.contains ident.idText ->
        checkFormattingFunctionApplication text range
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "InterpolatedStringWithNoSubstitution"
            Identifier = Identifiers.InterpolatedStringWithNoSubstitution
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
