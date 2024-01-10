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

let internal (|RecursiveFunctions|_|) (astNode: AstNode)  =
    match astNode with
    | AstNode.ModuleDeclaration (SynModuleDecl.Let (true, bindings, _)) ->
        let recursiveBindings =
            bindings 
            |> List.choose 
                (fun binding -> 
                    match binding with
                    | SynBinding (_, _, _, _, attributes, _, _, SynPat.LongIdent (SynLongIdent([ident], _, _), _, _, _, _, range), _, body, _, _, _) ->
                        Some { Identifier = ident; Range = range; Body = body; Attributes = attributes } 
                    | _ -> None)
        match recursiveBindings with
        | [] -> None
        | _ -> Some recursiveBindings
    | _ -> None

let internal functionIsCalledInOneOf (checkInfo: FSharpCheckFileResults)
                                     (callee: RecursiveFunctionInfo)
                                     (callers: list<RecursiveFunctionInfo>) =
    let calleeName = callee.Identifier.idText
    callers
    |> List.exists
        (fun caller ->
            checkInfo.GetAllUsesOfAllSymbolsInFile()
            |> Seq.exists (fun usage -> 
                usage.Symbol.DisplayName = calleeName 
                && ExpressionUtilities.rangeContainsOtherRange caller.Body.Range usage.Range))

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
    | RecursiveFunctions(funcs), Some checkInfo ->
        funcs 
            |> List.choose 
                (fun functionInfo -> 
                    if not (functionIsCalledInOneOf checkInfo functionInfo funcs) then
                        emitWarning functionInfo |> Some
                    else
                        None)
            |> List.toArray
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
