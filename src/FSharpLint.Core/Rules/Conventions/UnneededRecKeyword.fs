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
        | SynBinding (_, _, _, _, _, _, _, SynPat.LongIdent (SynLongIdent([ident], _, _), _, _, _, _, range), _, _, _, _, _) :: _ ->
            let symbolUses = checkInfo.GetAllUsesOfAllSymbolsInFile()
            let funcName = ident.idText

            let functionCalls =
                Seq.filter (fun (usage: FSharp.Compiler.CodeAnalysis.FSharpSymbolUse) ->
                    usage.Symbol.DisplayName = funcName
                    && usage.Range.StartLine >= letRange.StartLine
                    && usage.Range.EndLine <= letRange.EndLine) symbolUses


            if (Seq.length functionCalls) <= 1 then
                Array.singleton
                    {
                        Range = range
                        Message = String.Format(Resources.GetString "RulesUnneededRecKeyword", funcName)
                        SuggestedFix = None
                        TypeChecks = list.Empty
                    }
            else
                Array.empty
        | _ -> Array.empty

    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "UnneededRecKeyword"
            Identifier = Identifiers.UnneededRecKeyword
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
