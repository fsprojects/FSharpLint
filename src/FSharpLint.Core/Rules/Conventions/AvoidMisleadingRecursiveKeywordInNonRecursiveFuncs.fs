﻿module FSharpLint.Rules.AvoidMisleadingRecursiveKeywordInNonRecursiveFuncs

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

let runner (args: AstNodeRuleParams) =
    match args.AstNode, args.CheckInfo with
    | AstNode.ModuleDeclaration (SynModuleDecl.Let (isRecursive, bindings, _)), Some checkInfo when isRecursive ->
        match bindings with
        | SynBinding (_, _, _, _, _, _, _, SynPat.LongIdent (LongIdentWithDots([ident], _), _, _, _, _, range), _, _, _, _) :: _ ->
            let symbolUses = checkInfo.GetAllUsesOfAllSymbolsInFile()
            let funcName = ident.idText

            let functionCalls =
                symbolUses
                |> Seq.filter (fun (symbol) -> symbol.Symbol.DisplayName = funcName)

            if (functionCalls |> Seq.length) <= 1 then
                { Range = range
                  Message =
                    String.Format(
                        Resources.GetString "RulesAvoidMisleadingRecursiveKeywordInNonRecursiveFuncs",
                        funcName
                    )
                  SuggestedFix = None
                  TypeChecks = list.Empty }
                |> Array.singleton
            else
                Array.empty
        | _ -> Array.empty

    | _ -> Array.empty

let rule =
    { Name = "AvoidMisleadingRecursiveKeywordInNonRecursiveFuncs"
      Identifier = Identifiers.AvoidMisleadingRecursiveKeywordInNonRecursiveFuncs
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule