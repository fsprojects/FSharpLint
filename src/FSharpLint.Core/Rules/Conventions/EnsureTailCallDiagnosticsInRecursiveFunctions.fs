module FSharpLint.Rules.EnsureTailCallDiagnosticsInRecursiveFunctions

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    let emitWarning (isNested: bool) (func: UnneededRecKeyword.RecursiveFunctionInfo) =
        let message = 
            String.Format(
                Resources.GetString "RulesEnsureTailCallDiagnosticsInRecursiveFunctions",
                func.Identifier.idText
            )
        { Range = func.Range
          Message = 
            if isNested then 
                sprintf "%s %s" message (Resources.GetString "RulesEnsureTailCallDiagnosticsInRecursiveFunctionsNested") 
            else
                message
          SuggestedFix = None
          TypeChecks = list.Empty }

    let processFunction isNested checkInfo funcs functionInfo =
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
                emitWarning isNested functionInfo |> Some
        else
            None

    match (args.AstNode, args.CheckInfo) with
    | UnneededRecKeyword.RecursiveFunctions(funcs), Some checkInfo ->
        funcs
        |> List.choose (processFunction false checkInfo funcs)
        |> List.toArray
    | AstNode.Expression(SynExpr.LetOrUse(true, _, bindings, _, _, _)), Some checkInfo ->
        match UnneededRecKeyword.getRecursiveFunctionsFromBindings bindings with
        | [] -> Array.empty
        | funcs -> 
            funcs
            |> List.choose (processFunction true checkInfo funcs)
            |> List.toArray
    | _ -> Array.empty

let rule =
    AstNodeRule
        { Name = "EnsureTailCallDiagnosticsInRecursiveFunctions"
          Identifier = Identifiers.EnsureTailCallDiagnosticsInRecursiveFunctions
          RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
