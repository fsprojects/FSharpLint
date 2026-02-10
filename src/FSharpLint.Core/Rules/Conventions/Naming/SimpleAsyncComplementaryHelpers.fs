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
    | Async of typeParam: Option<SynType> * isLowercase: bool
    | Task of typeParam: Option<SynType>

type private Func =
    {
        BaseName: string
        Range: range
        ReturnType: ReturnType
        ArgumentsString: string
    }

[<TailCall>]
let rec private getBindings (acc: list<SynBinding>) (declarations: list<SynModuleDecl>) =
    match declarations with
    | SynModuleDecl.Let(_, bindings, _) :: rest -> getBindings (bindings @ acc) rest
    | SynModuleDecl.NestedModule(_, _, innerDecls, _, _, _) :: rest -> getBindings acc (innerDecls @ rest)
    | [] -> acc
    | _ :: rest -> getBindings acc rest

[<TailCall>]
let rec private extractArgIdentRange (pattern: SynPat) =
    match pattern with
    | SynPat.Paren(pat, _) -> extractArgIdentRange pat
    | SynPat.Typed(pat, _, _) -> pat.Range
    | _ -> pattern.Range

let runner (args: AstNodeRuleParams) =
    let emitWarning (func: Func) =
        let funcDefinitionString = 
            match ExpressionUtilities.tryFindTextOfRange func.Range args.FileContent with
            | Some text -> text
            | None -> failwithf "Invalid range: %A" func.Range

        let tryGetTypeParamString (maybeTypeParam: Option<SynType>) =
            match maybeTypeParam with
            | Some typeParam -> ExpressionUtilities.tryFindTextOfRange typeParam.Range args.FileContent
            | None -> None

        let message =
            match func.ReturnType with
            | Async (typeParam, isLowercase) -> 
                let prefix =
                    if isLowercase then
                        asyncSuffixOrPrefix.ToLowerInvariant()
                    else
                        asyncSuffixOrPrefix
                let newFuncName = funcDefinitionString.Replace(prefix + func.BaseName, func.BaseName + asyncSuffixOrPrefix)
                let newFuncNameWithProperCase =
                    if isLowercase then
                        sprintf "%c%s" (Char.ToLowerInvariant newFuncName.[0]) (newFuncName.Substring 1)
                    else
                        newFuncName
                let typeDefString =
                    match tryGetTypeParamString typeParam with
                    | Some "unit" -> ": Task"
                    | Some str -> $": Task<{str}>"
                    | None -> String.Empty
                String.Format(
                    Resources.GetString "RulesSimpleAsyncComplementaryHelpersAsync",
                    newFuncNameWithProperCase,
                    typeDefString,
                    prefix,
                    func.BaseName,
                    func.ArgumentsString
                )
            | Task typeParam ->
                let newFuncName = funcDefinitionString.Replace(func.BaseName + asyncSuffixOrPrefix, asyncSuffixOrPrefix + func.BaseName)
                let typeDefString =
                    match tryGetTypeParamString typeParam with
                    | Some str -> $": Async<{str}>"
                    | None -> String.Empty
                String.Format(
                    Resources.GetString "RulesSimpleAsyncComplementaryHelpersTask",
                    newFuncName,
                    typeDefString,
                    func.BaseName,
                    func.ArgumentsString
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

        let tryGetFunction (binding: SynBinding) =
            match binding with
            | SynBinding(_, _, _, _, _, _, _, SynPat.LongIdent(funcIdent, _, _, argPats, (None | Some(SynAccess.Public _)), _), returnInfo, _, _, _, _)
                when not argPats.Patterns.IsEmpty ->
                let returnTypeParam =
                    match returnInfo with
                    | Some(SynBindingReturnInfo(SynType.App(SynType.LongIdent(SynLongIdent _), _, [ typeParam ], _, _, _, _), _, _, _)) ->
                        Some typeParam 
                    | _ -> None
                
                let funcArgs =
                    match argPats with
                    | SynArgPats.NamePatPairs(pairs, _, _) ->
                        let argsList =
                            pairs
                            |> List.map (fun (ident, _, _) -> ident.idRange)
                            |> List.choose (fun range -> ExpressionUtilities.tryFindTextOfRange range args.FileContent)
                        " " + String.Join(" ", argsList)
                    | SynArgPats.Pats([ SynPat.Paren(SynPat.Const(SynConst.Unit, _), _) ]) ->
                        "()"
                    | SynArgPats.Pats([ SynPat.Paren(SynPat.Tuple(_, pats, _, _), _) ]) ->
                        let argsList =
                            pats
                            |> List.map extractArgIdentRange
                            |> List.choose (fun range -> ExpressionUtilities.tryFindTextOfRange range args.FileContent)
                        sprintf "(%s)" (String.Join(", ", argsList))
                    | SynArgPats.Pats(pats) ->
                        let argsList =
                            pats
                            |> List.map extractArgIdentRange
                            |> List.choose (fun range -> ExpressionUtilities.tryFindTextOfRange range args.FileContent)
                        " " + String.Join(" ", argsList)

                match funcIdent with
                | HasAsyncPrefix name ->
                    Some
                        { 
                            BaseName = name.Substring asyncSuffixOrPrefix.Length
                            Range = binding.RangeOfHeadPattern
                            ReturnType = Async(returnTypeParam, Char.IsLower name.[0])
                            ArgumentsString = funcArgs
                        }
                | HasAsyncSuffix name ->
                    Some
                        {
                            BaseName = name.Substring(0, name.Length - asyncSuffixOrPrefix.Length)
                            Range = binding.RangeOfHeadPattern
                            ReturnType = Task returnTypeParam
                            ArgumentsString = funcArgs
                        }
                | HasNoAsyncPrefixOrSuffix _ ->
                    None
            | _ -> None

        let funcs = bindings |> List.choose tryGetFunction

        let asyncFuncs = funcs |> List.filter (fun func -> func.ReturnType.IsAsync)
        let taskFuncs = funcs |> List.filter (fun func -> func.ReturnType.IsTask)
        
        let haveSameBaseName (func: Func) (otherFunc: Func) =
            otherFunc.BaseName.Equals(func.BaseName, StringComparison.InvariantCultureIgnoreCase)

        let checkFuncs (targetFuncs: list<Func>) (otherFuncs: list<Func>) =
            targetFuncs
            |> List.map
                (fun func ->
                    if otherFuncs |> List.exists (haveSameBaseName func) then
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
