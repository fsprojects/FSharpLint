module FSharpLint.Rules.EnsureTailCallDiagnosticsInRecursiveFunctions

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (args: AstNodeRuleParams) =
    match args.AstNode, args.CheckInfo with
    | AstNode.ModuleDeclaration (SynModuleDecl.Let (true, bindings, _)), Some checkInfo ->
        match bindings with
        | SynBinding (_, _, _, _, attributes, _, _, SynPat.LongIdent (SynLongIdent([ident], _, _), _, _, _, _, range), _, body, _, _, _) :: _ ->
            let funcName = ident.idText
        
            let functionCallsItself =
                checkInfo.GetAllUsesOfAllSymbolsInFile()
                |> Seq.exists (fun usage -> 
                    usage.Symbol.DisplayName = funcName 
                    && ExpressionUtilities.rangeContainsOtherRange body.Range usage.Range)

            if functionCallsItself then
                let hasTailCallAttribute =
                    attributes 
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
                    { Range = range
                      Message =
                        String.Format(
                            Resources.GetString "RulesEnsureTailCallDiagnosticsInRecursiveFunctions",
                            funcName
                        )
                      SuggestedFix = None
                      TypeChecks = list.Empty }
                    |> Array.singleton
            else
                { Range = range
                  Message =
                    String.Format(
                        Resources.GetString "RulesUnneededRecKeyword",
                        funcName
                    )
                  SuggestedFix = None
                  TypeChecks = list.Empty }
                |> Array.singleton
        | _ -> Array.empty
    | _ -> Array.empty

let rule =
    { Name = "EnsureTailCallDiagnosticsInRecursiveFunctions"
      Identifier = Identifiers.EnsureTailCallDiagnosticsInRecursiveFunctions
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
