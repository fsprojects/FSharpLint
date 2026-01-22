module FSharpLint.Rules.FavourNestedFunctions

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
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
    let collectTopLevelFunctionBindings (declaration: SynModuleDecl): list<FunctionBinding> = 
        match declaration with
        | SynModuleDecl.Let(_, bindings, _) -> collectBindings bindings
        | _ -> List.empty
    
    match args.AstNode with
    | AstNode.ModuleOrNamespace(SynModuleOrNamespace(_, _, _kind, declarations, _, _, _, moduleRange, _)) ->
        let allTopLevelFunctionBindingsInModule =
            declarations |> List.collect collectTopLevelFunctionBindings
        
        let privateFunctionIdentifiers = 
            allTopLevelFunctionBindingsInModule
            |> Seq.choose 
                (fun funcBinding ->
                    match funcBinding.Accessibility with
                    | Some(SynAccess.Private _) when extractAttributes funcBinding.Attributes |> List.isEmpty -> 
                        Some funcBinding.Identifier
                    | _ -> None)
            |> Seq.toArray

        let collectFunctionBindings (node: AstNode) =
            match node with
            | AstNode.Binding(binding) 
                when ExpressionUtilities.rangeContainsOtherRange moduleRange binding.RangeOfBindingWithRhs ->
                collectBindings (List.singleton binding)
            | AstNode.MemberDefinition(memberDefn)
                when ExpressionUtilities.rangeContainsOtherRange moduleRange memberDefn.Range ->
                match memberDefn with
                | SynMemberDefn.Member(binding, _)  ->
                    collectBindings (List.singleton binding)
                | SynMemberDefn.GetSetMember(getMember, setMember, _, _)  ->
                    collectBindings ((Option.toList getMember) @ (Option.toList setMember))
                | SynMemberDefn.LetBindings(bindings, _, _, _)  ->
                    collectBindings bindings
                | SynMemberDefn.Interface(_, _, Some(members), _) ->
                    collectMemberBindings List.empty members
                | _  -> List.empty
            | AstNode.LambdaBody(bodyExpr: SynExpr)
                when ExpressionUtilities.rangeContainsOtherRange moduleRange bodyExpr.Range ->
                List.singleton { Identifier = Ident("<lambda>", range()); Expression = bodyExpr; Accessibility = None; Attributes = List.empty }
            | _ -> List.empty

        match args.CheckInfo with
        | Some checkInfo when privateFunctionIdentifiers.Length > 0 ->  
            let allFunctionsInModule =
                args.SyntaxArray
                |> Array.toList
                |> List.collect (fun node -> collectFunctionBindings node.Actual)
                |> List.distinctBy (fun funcBinding -> funcBinding.Expression.Range)

            let contains oneBinding anotherBinding =
                oneBinding.Expression.Range <> anotherBinding.Expression.Range
                &&
                ExpressionUtilities.rangeContainsOtherRange 
                    oneBinding.Expression.Range 
                    anotherBinding.Expression.Range
            
            // If a function is nested in another function or method and both use the same private
            // function, treat them as one function when counting number of functions in which a
            // function is used.
            let allFunctionsInModuleExcludingNested =
                allFunctionsInModule
                |> List.filter 
                    (fun funcBinding -> 
                        allFunctionsInModule 
                        |> List.forall 
                            (fun otherBinding -> contains otherBinding funcBinding |> not))

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
                        allFunctionsInModuleExcludingNested
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
