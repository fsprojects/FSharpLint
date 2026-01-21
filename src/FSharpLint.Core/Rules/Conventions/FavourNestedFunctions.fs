module FSharpLint.Rules.FavourNestedFunctions

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

type private FunctionBinding =
    {
        Identifier: Ident
        Expression: SynExpr
        Accessibility: Option<SynAccess>
        Attributes: SynAttributes
    }

let private collectBindings (bindings: list<SynBinding>) =
    bindings
    |> List.choose
        (fun binding ->
            match binding with
            | SynBinding(_, _, _, _, attributes, _, _, SynPat.LongIdent(SynLongIdent(idents, _, _), _, _, _, accessibility, _), _, expr, _, _, _) ->
                Some { Identifier = List.last idents; Expression = expr; Accessibility = accessibility; Attributes = attributes }
            | _ -> None
        )

[<TailCall>]
let rec private collectMemberBindings (acc: list<FunctionBinding>) (memberDefns: list<SynMemberDefn>): list<FunctionBinding> =
    match memberDefns with
    | [] ->
        acc
    | SynMemberDefn.Member(binding, _) :: rest ->
        collectMemberBindings (collectBindings (List.singleton binding) @ acc) rest
    | SynMemberDefn.GetSetMember(getMember, setMember, _, _) :: rest ->
        collectMemberBindings (collectBindings ((Option.toList getMember) @ (Option.toList setMember)) @ acc) rest
    | SynMemberDefn.LetBindings(bindings, _, _, _) :: rest ->
        collectMemberBindings (collectBindings bindings @ acc) rest
    | SynMemberDefn.Interface(_, _, Some(members), _) :: rest ->
        collectMemberBindings acc (members @ rest)
    | _ :: rest ->
        collectMemberBindings acc rest

let runner (args: AstNodeRuleParams) =
    let getFunctionBindings (declaration: SynModuleDecl) = 
        let collectBindingsInType (typeDefn: SynTypeDefn) =
            match typeDefn with
            | SynTypeDefn(_, SynTypeDefnRepr.ObjectModel(_, members, _), _, _, _, _) ->
                collectMemberBindings List.empty members
            | _ -> List.empty

        match declaration with
        | SynModuleDecl.Let(_, bindings, _) ->
            collectBindings bindings
        | SynModuleDecl.Types(typeDefns, _) ->
            typeDefns |> List.collect collectBindingsInType
        | _ -> List.Empty

    match args.AstNode with
    | AstNode.ModuleOrNamespace(SynModuleOrNamespace(_, _, _kind, declarations, _, _, _, _, _)) ->
        let privateFunctionIdentifiers = 
            declarations
            |> Seq.collect getFunctionBindings
            |> Seq.choose 
                (fun funcBinding ->
                    match funcBinding.Accessibility with
                    | Some(SynAccess.Private _) when extractAttributes funcBinding.Attributes |> List.isEmpty -> 
                        Some funcBinding.Identifier
                    | _ -> None)
            |> Seq.toArray

        match args.CheckInfo with
        | Some checkInfo when privateFunctionIdentifiers.Length > 0 ->
            let allFunctionBindingsInModule =
                declarations
                |> List.collect getFunctionBindings
            
            let emitWarningIfNeeded currFunctionIdentifier =
                match ExpressionUtilities.getSymbolFromIdent args.CheckInfo (SynExpr.Ident currFunctionIdentifier) with
                | Some symbolUse ->
                    let allSymbolUses = checkInfo.GetUsesOfSymbolInFile symbolUse.Symbol
                    let functionUsesCurrFunction (funcBinding: FunctionBinding) =
                        if funcBinding.Identifier.idText = currFunctionIdentifier.idText then
                            false
                        else
                            allSymbolUses
                            |> Array.exists 
                                (fun usage -> 
                                    ExpressionUtilities.rangeContainsOtherRange
                                        funcBinding.Expression.Range
                                        usage.Range)

                    let numberOfOtherFunctionsCurrFunctionIsUsedIn =
                        allFunctionBindingsInModule
                        |> Seq.filter functionUsesCurrFunction
                        |> Seq.length

                    if numberOfOtherFunctionsCurrFunctionIsUsedIn = 1 then
                        Some {
                            Range = currFunctionIdentifier.idRange
                            WarningDetails.Message = Resources.GetString "RulesFavourNestedFunctions"
                            SuggestedFix = None
                            TypeChecks = List.Empty
                        }
                    else
                        None
                | None -> None

            privateFunctionIdentifiers
            |> Array.choose emitWarningIfNeeded
        | _ -> Array.empty
    | _ -> Array.empty

let rule =
    { Name = "FavourNestedFunctions"
      Identifier = Identifiers.FavourNestedFunctions
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
