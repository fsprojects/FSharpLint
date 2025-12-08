module FSharpLint.Rules.EnsureTailCallDiagnosticsInRecursiveFunctions

open System

open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private generateViolation (func: UnneededRecKeyword.RecursiveFunctionInfo) =
    { Range = func.Range
      Message =
        String.Format(
            Resources.GetString "RulesEnsureTailCallDiagnosticsInRecursiveFunctions",
            func.Identifier.idText
        )
      SuggestedFix = None
      TypeChecks = list.Empty }

let runner (args: AstNodeRuleParams) =
    match (args.AstNode, args.CheckInfo) with
    | UnneededRecKeyword.RecursiveFunctions(funcs), Some checkInfo ->
        let processFunction functionInfo =
            if UnneededRecKeyword.functionIsCalledInOneOf checkInfo functionInfo funcs then
                let hasTailCallAttribute =
                    functionInfo.Attributes 
                    |> List.collect (fun attrs -> attrs.Attributes) 
                    |> List.exists 
                        (fun attr -> 
                            match attr.TypeName with
                            | SynLongIdent([ident], _, _) ->
                                ident.idText = "TailCall" || ident.idText = "TailCallAttribute"
                            | _ -> false)
                if hasTailCallAttribute then
                    None
                else
                    generateViolation functionInfo |> Some
            else
                None
        funcs
        |> List.choose processFunction
        |> List.toArray
    | _ -> Array.empty

let rule =
    AstNodeRule
        { Name = "EnsureTailCallDiagnosticsInRecursiveFunctions"
          Identifier = Identifiers.EnsureTailCallDiagnosticsInRecursiveFunctions
          RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
