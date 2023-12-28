module FSharpLint.Rules.UnneededRecKeyword

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

let runner (args: AstNodeRuleParams) =
    match args.AstNode, args.CheckInfo with
    | AstNode.ModuleDeclaration (SynModuleDecl.Let (isRecursive, bindings, letRange)), Some checkInfo when isRecursive ->
        match bindings with
        | SynBinding (_, _, _, _, _, _, _, SynPat.LongIdent (LongIdentWithDots([ident], _), _, _, _, _, range), _, _, _, _) :: _ ->
            let symbolUses = checkInfo.GetAllUsesOfAllSymbolsInFile()
            let funcName = ident.idText

            let functionCalls =
                symbolUses
                |> Seq.filter (fun usage ->
                    usage.Symbol.DisplayName = funcName
                    && usage.Range.StartLine >= letRange.StartLine
                    && usage.Range.EndLine <= letRange.EndLine)


            if (functionCalls |> Seq.length) <= 1 then
                { Range = range
                  Message =
                    String.Format(
                        Resources.GetString "RulesUnneededRecKeyword",
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
    { Name = "UnneededRecKeyword"
      Identifier = Identifiers.UnneededRecKeyword
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
