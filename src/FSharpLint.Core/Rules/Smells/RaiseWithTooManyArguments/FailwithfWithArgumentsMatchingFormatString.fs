module FSharpLint.Rules.FailwithfWithArgumentsMatchingFormatString

open System

open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private runner (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.App(_, false, _, _, _)) as expr ->
        match expr with
        | FuncApp(expressions, range) ->
            match expressions with
            | SynExpr.Ident(ident)::SynExpr.Const(SynConst.String(formatString, _, _), _)::arguments
                when ident.idText = "failwithf" && List.length arguments = formatString.Replace("%%", String.Empty).Split('%').Length ->
                Array.singleton
                    {
                        Range = range
                        Message = Resources.GetString "FailwithfWithArgumentsMatchingFormatString"
                        SuggestedFix = None
                        TypeChecks = List.Empty
                    }
            | _ -> Array.empty
        | _ -> Array.empty
    | _ -> Array.empty


let rule =
    AstNodeRule
        {
            Name = "FailwithfWithArgumentsMatchingFormatString"
            Identifier = Identifiers.FailwithfWithArgumentsMatchingFormattingString
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
