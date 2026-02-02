module FSharpLint.Rules.SimpleAsyncComplementaryHelpers

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open Helper.Naming.Asynchronous
open FSharp.Compiler.Syntax

[<TailCall>]
let rec private getBindings (acc: list<SynBinding>) (declarations: list<SynModuleDecl>) =
    match declarations with
    | SynModuleDecl.Let(_, bindings, _) :: rest -> getBindings (bindings @ acc) rest
    | SynModuleDecl.NestedModule(_, _, innerDecls, _, _, _) :: rest -> getBindings acc (innerDecls @ rest)
    | [] -> acc
    | _ :: rest -> getBindings acc rest

let runner (args: AstNodeRuleParams) =
    let emitWarning range missingFunctionName =
        Array.singleton
            {
                Range = range
                Message = $"Create {missingFunctionName}"
                SuggestedFix = None
                TypeChecks = List.empty
            }

    let processDeclarations (declarations: list<SynModuleDecl>) =
        let bindings = getBindings List.Empty declarations

        let funcsGrouped = 
            bindings
            |> List.choose
                (fun binding ->
                    match binding with
                    | SynBinding(_, _, _, _, _, _, _, SynPat.LongIdent(funcIdent, _, _, _, _, _), _, _, _, _, _) ->
                        match funcIdent with
                        | HasAsyncPrefix name -> Some(0, funcIdent, name.Substring asyncSuffixOrPrefix.Length)
                        | HasAsyncSuffix name -> Some(1, funcIdent, name.Substring(0, name.Length - asyncSuffixOrPrefix.Length))
                        | HasNoAsyncPrefixOrSuffix _ -> None
                    | _ -> None)
            |> List.groupBy (fun (key, _, _) -> key)
            |> Map.ofList

        let asyncFuncs = funcsGrouped |> Map.tryFind 0 |> Option.defaultValue List.Empty
        let taskFuncs = funcsGrouped |> Map.tryFind 1 |> Option.defaultValue List.Empty
        
        let asyncFunWarnings =
            asyncFuncs
            |> List.map
                (fun (_, ident, baseName) ->
                    if taskFuncs |> List.exists (fun (_, _, otherBaseName) -> baseName = otherBaseName) then
                        Array.empty
                    else
                        emitWarning ident.Range (baseName + asyncSuffixOrPrefix))
            |> Array.concat

        let taskFunWarnings =
            taskFuncs
            |> List.map
                (fun (_, ident, baseName) ->
                    if asyncFuncs |> List.exists (fun (_, _, otherBaseName) -> baseName = otherBaseName) then
                        Array.empty
                    else
                        emitWarning ident.Range (asyncSuffixOrPrefix + baseName))
            |> Array.concat
        
        Array.append asyncFunWarnings taskFunWarnings

    match args.AstNode with
    | Ast.ModuleOrNamespace(SynModuleOrNamespace(_, _, _, declarations, _, _, _, _, _)) ->
        processDeclarations declarations
    | ModuleDeclaration(SynModuleDecl.NestedModule(_, _, declarations, _, _, _)) ->
        processDeclarations declarations
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "SimpleAsyncComplementaryHelpers"
            Identifier = Identifiers.SimpleAsyncComplementaryHelpers
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
