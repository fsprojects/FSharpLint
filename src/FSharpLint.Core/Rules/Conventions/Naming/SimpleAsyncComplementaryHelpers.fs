module FSharpLint.Rules.SimpleAsyncComplementaryHelpers

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open Helper.Naming.Asynchronous
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type private ReturnType =
    | Async
    | AsyncUnit
    | Task

type private Func =
    {
        BaseName: string
        Range: range
        ReturnType: ReturnType
    }

[<TailCall>]
let rec private getBindings (acc: list<SynBinding>) (declarations: list<SynModuleDecl>) =
    match declarations with
    | SynModuleDecl.Let(_, bindings, _) :: rest -> getBindings (bindings @ acc) rest
    | SynModuleDecl.NestedModule(_, _, innerDecls, _, _, _) :: rest -> getBindings acc (innerDecls @ rest)
    | [] -> acc
    | _ :: rest -> getBindings acc rest

let runner (args: AstNodeRuleParams) =
    let emitWarning (func: Func) =
        let funcDefinitionString = 
            match ExpressionUtilities.tryFindTextOfRange func.Range args.FileContent with
            | Some text -> text
            | None -> failwithf "Invalid range: %A" func.Range

        let message =
            match func.ReturnType with
            | Async -> 
                let newFuncName = funcDefinitionString.Replace(asyncSuffixOrPrefix + func.BaseName, func.BaseName + asyncSuffixOrPrefix)
                String.Format(
                    Resources.GetString "RulesSimpleAsyncComplementaryHelpersAsync",
                    newFuncName,
                    String.Empty,
                    asyncSuffixOrPrefix,
                    func.BaseName,
                    "()"
                )
            | AsyncUnit ->
                let newFuncName = funcDefinitionString.Replace(asyncSuffixOrPrefix + func.BaseName, func.BaseName + asyncSuffixOrPrefix)
                String.Format(
                    Resources.GetString "RulesSimpleAsyncComplementaryHelpersAsync",
                    newFuncName,
                    ": Task",
                    asyncSuffixOrPrefix,
                    func.BaseName,
                    "()"
                )
            | Task ->
                let newFuncName = funcDefinitionString.Replace(func.BaseName + asyncSuffixOrPrefix, asyncSuffixOrPrefix + func.BaseName)
                String.Format(
                    Resources.GetString "RulesSimpleAsyncComplementaryHelpersTask",
                    newFuncName,
                    String.Empty,
                    func.BaseName,
                    "()"
                )

        Array.singleton
            {
                Range = func.Range
                Message = message
                SuggestedFix = None
                TypeChecks = List.empty
            }


    let processDeclarations (declarations: list<SynModuleDecl>) =
        let bindings = getBindings List.Empty declarations

        let funcs = 
            bindings
            |> List.choose
                (fun binding ->
                    match binding with
                    | SynBinding(_, _, _, _, _, _, _, SynPat.LongIdent(funcIdent, _, _, _, (None | Some(SynAccess.Public _)), _), returnInfo, _, range, _, _) ->
                        match funcIdent with
                        | HasAsyncPrefix name ->
                            let returnType =
                                match returnInfo with
                                | Some(SynBindingReturnInfo(SynType.App(SynType.LongIdent(SynLongIdent(_, _, _)), _, [ SynType.LongIdent (SynLongIdent ([ unitIdent ], [], [None])) ], _, _, _, _), _, _, _))
                                    when unitIdent.idText = "unit" ->
                                    AsyncUnit
                                | _ -> Async

                            Some
                                { 
                                    BaseName = name.Substring asyncSuffixOrPrefix.Length
                                    Range = range
                                    ReturnType = returnType
                                }
                        | HasAsyncSuffix name ->
                            Some
                                {
                                    BaseName = name.Substring(0, name.Length - asyncSuffixOrPrefix.Length)
                                    Range = range
                                    ReturnType = Task
                                }
                        | HasNoAsyncPrefixOrSuffix _ ->
                            None
                    | _ -> None)

        let asyncFuncs = funcs |> List.filter (fun func -> func.ReturnType <> Task)
        let taskFuncs = funcs |> List.filter (fun func -> func.ReturnType = Task)
        
        let checkFuncs (targetFuncs: list<Func>) (otherFuncs: list<Func>) =
            targetFuncs
            |> List.map
                (fun func ->
                    if otherFuncs |> List.exists (fun otherFunc -> otherFunc.BaseName = func.BaseName) then
                        Array.empty
                    else
                        emitWarning func)
            |> Array.concat
        
        Array.append (checkFuncs asyncFuncs taskFuncs) (checkFuncs taskFuncs asyncFuncs)

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
