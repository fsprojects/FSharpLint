module FSharpLint.Rules.FavourNestedFunctions

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

let runner (args: AstNodeRuleParams) =
    let getFunctionBindings (declaration: SynModuleDecl) = 
        match declaration with
        | SynModuleDecl.Let(_, bindings, _) ->
            bindings
            |> List.choose
                (fun binding ->
                    match binding with
                    | SynBinding(_, _, _, _, _, _, _, SynPat.LongIdent(SynLongIdent([ident], _, _), _, _, _, accessibility, _), _, expr, _, _, _) ->
                        Some(ident, expr, accessibility)
                    | _ -> None
                )
        | _ -> List.Empty

    match args.AstNode with
    | AstNode.ModuleOrNamespace(SynModuleOrNamespace(_, _, _kind, declarations, _, _, _, _, _)) ->
        let privateFunctionIdentifiers = 
            declarations
            |> Seq.collect getFunctionBindings
            |> Seq.choose 
                (fun (ident, _expr, accessibility) ->
                    match accessibility with
                    | Some(SynAccess.Private _) -> 
                        Some ident
                    | _ -> None)
            |> Seq.toArray

        match args.CheckInfo with
        | Some checkInfo when privateFunctionIdentifiers.Length > 0 ->
            let otherFunctionBodies =
                declarations
                |> Seq.collect getFunctionBindings
                |> Seq.choose 
                    (fun (ident, body, _accessibility) ->
                        if not(Array.exists (fun (each: Ident) -> each.idText = ident.idText) privateFunctionIdentifiers) then 
                            Some body
                        else
                            None)
            
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
