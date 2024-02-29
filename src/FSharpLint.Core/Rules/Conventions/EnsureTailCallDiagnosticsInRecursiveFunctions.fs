module FSharpLint.Rules.EnsureTailCallDiagnosticsInRecursiveFunctions

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private emitWarning (func: UnneededRecKeyword.RecursiveFunctionInfo) =
    { Range = func.Range
      Message =
        String.Format(
            Resources.GetString "RulesEnsureTailCallDiagnosticsInRecursiveFunctions",
            func.Identifier.idText
        )
      SuggestedFix = None
      TypeChecks = list.Empty }

let runner (args: AstNodeRuleParams) =
    match args.AstNode, args.CheckInfo with
    | UnneededRecKeyword.RecursiveFunction(func), Some checkInfo ->
        if UnneededRecKeyword.functionCallsItself checkInfo func then
            let hasTailCallAttribute =
                func.Attributes 
                |> List.collect (fun attrs -> attrs.Attributes) 
                |> List.exists 
                    (fun attr -> 
                        match attr.TypeName with
                        | SynLongIdent([ident], _, _) ->
                            ident.idText = "TailCall" || ident.idText = "TailCallAttribute"
                        | _ -> false)
            if hasTailCallAttribute then
                Array.empty
            else
                emitWarning func |> Array.singleton
        else
            Array.empty
    | _ -> Array.empty

let rule =
    { Name = "EnsureTailCallDiagnosticsInRecursiveFunctions"
      Identifier = Identifiers.EnsureTailCallDiagnosticsInRecursiveFunctions
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
