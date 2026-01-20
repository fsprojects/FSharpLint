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

let runner (args: AstNodeRuleParams) =
    let getFunctionBindings (declaration: SynModuleDecl) = 
        match declaration with
        | SynModuleDecl.Let(_, bindings, _) ->
            bindings
            |> List.choose
                (fun binding ->
                    match binding with
                    | SynBinding(_, _, _, _, attributes, _, _, SynPat.LongIdent(SynLongIdent([ident], _, _), _, _, _, accessibility, _), _, expr, _, _, _) ->
                        Some { Identifier = ident; Expression = expr; Accessibility = accessibility; Attributes = attributes }
                    | _ -> None
                )
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
            let otherFunctionBodies =
                declarations
                |> Seq.collect getFunctionBindings
                |> Seq.choose 
                    (fun funcBinding ->
                        let isOneOfPrivateFunctions =
                            Array.exists
                                (fun (each: Ident) -> each.idText = funcBinding.Identifier.idText)
                                privateFunctionIdentifiers

                        if not isOneOfPrivateFunctions then Some funcBinding.Expression
                        else None)
            
            let emitWarningIfNeeded currFunctionIdentifier =
                match ExpressionUtilities.getSymbolFromIdent args.CheckInfo (SynExpr.Ident currFunctionIdentifier) with
                | Some symbolUse ->
                    let numberOfOtherFunctionsCurrFunctionIsUsedIn =
                        otherFunctionBodies
                        |> Seq.filter (fun funcBody -> 
                            checkInfo.GetUsesOfSymbolInFile symbolUse.Symbol
                            |> Array.exists (fun usage -> ExpressionUtilities.rangeContainsOtherRange funcBody.Range usage.Range))
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
