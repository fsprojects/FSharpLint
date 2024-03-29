module FSharpLint.Rules.ModuleDeclSpacing

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.ExpressionUtilities

let checkModuleDeclSpacing (args:AstNodeRuleParams) synModuleOrNamespace =
    match synModuleOrNamespace with
    | SynModuleOrNamespace (_, _, _, decls, _, _, _, _, _) ->
        decls
        |> List.toArray
        |> Array.pairwise
        |> Array.choose (fun (declOne, declTwo) ->
            let numPrecedingCommentLines = countPrecedingCommentLines args.FileContent declOne.Range.End declTwo.Range.Start
            if declTwo.Range.StartLine <> declOne.Range.EndLine + 3 + numPrecedingCommentLines then
                let intermediateRange =
                    let startLine = declOne.Range.EndLine + 1
                    let endLine = declTwo.Range.StartLine
                    let endOffset =
                        if startLine = endLine
                        then 1
                        else 0

                    Range.mkRange
                        ""
                        (Position.mkPos (declOne.Range.EndLine + 1) 0)
                        (Position.mkPos (declTwo.Range.StartLine + endOffset) 0)
                { Range = intermediateRange
                  Message = Resources.GetString("RulesFormattingModuleDeclSpacingError")
                  SuggestedFix = None
                  TypeChecks = [] } |> Some
            else
                None)

let runner args =
    match args.AstNode with
    | AstNode.ModuleOrNamespace synModuleOrNamespace ->
        checkModuleDeclSpacing args synModuleOrNamespace
    | _ -> Array.empty

let rule =
    { Name = "ModuleDeclSpacing"
      Identifier = Identifiers.ModuleDeclSpacing
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
