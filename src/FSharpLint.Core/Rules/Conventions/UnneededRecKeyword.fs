module FSharpLint.Rules.UnneededRecKeyword

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

type internal RecursiveFunctionInfo =
    {
        Identifier: Ident
        Range: range
        Body: SynExpr
        Attributes: SynAttributes
    }

let internal (|RecursiveFunction|_|) (astNode: AstNode)  =
    match astNode with
    | AstNode.ModuleDeclaration (SynModuleDecl.Let (true, bindings, _)) ->
        match bindings with
        | SynBinding (_, _, _, _, attributes, _, _, SynPat.LongIdent (SynLongIdent([ident], _, _), _, _, _, _, range), _, body, _, _, _) :: _ ->
            Some { Identifier = ident; Range = range; Body = body; Attributes = attributes } 
        | _ -> None
    | _ -> None

let internal functionCallsItself (checkInfo: FSharpCheckFileResults) (func: RecursiveFunctionInfo) =
    let funcName = func.Identifier.idText
    checkInfo.GetAllUsesOfAllSymbolsInFile()
    |> Seq.exists (fun usage -> 
        usage.Symbol.DisplayName = funcName 
        && ExpressionUtilities.rangeContainsOtherRange func.Body.Range usage.Range)

let private emitWarning (func: RecursiveFunctionInfo) =
    { Range = func.Range
      Message =
          String.Format(
              Resources.GetString "RulesUnneededRecKeyword",
              func.Identifier.idText
          )
      SuggestedFix = None
      TypeChecks = list.Empty }

let runner (args: AstNodeRuleParams) =
    match args.AstNode, args.CheckInfo with
    | RecursiveFunction(func), Some checkInfo when not (functionCallsItself checkInfo func) ->
        emitWarning func |> Array.singleton
    | _ -> Array.empty

let rule =
    { Name = "UnneededRecKeyword"
      Identifier = Identifiers.UnneededRecKeyword
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
