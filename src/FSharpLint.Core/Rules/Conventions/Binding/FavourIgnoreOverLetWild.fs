module FSharpLint.Rules.FavourIgnoreOverLetWild

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private checkForBindingToAWildcard pattern range fileContent (expr: SynExpr) letBindingRange =
    let rec findWildAndIgnoreParens = function
        | SynPat.Paren(pattern, _) -> findWildAndIgnoreParens pattern
        | SynPat.Wild(_) -> true
        | _ -> false

    match ExpressionUtilities.tryFindTextOfRange expr.Range fileContent with
    | Some exprText -> 
        if findWildAndIgnoreParens pattern then
            { Range = range
              Message = Resources.GetString("RulesFavourIgnoreOverLetWildError")
              SuggestedFix = Some (lazy (Some({ FromRange = letBindingRange
                                                FromText = fileContent
                                                ToText = sprintf "(%s) |> ignore" exprText })))
              TypeChecks = [] } |> Array.singleton
        else
            Array.empty
    | None -> Array.empty

let private runner (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, _, pattern, _, expr, range, _)) ->
        let bindingRange  = 
            match args.GetParents(args.NodeIndex) with
            | AstNode.ModuleDeclaration(SynModuleDecl.Let(_, _, range)) :: _
            | AstNode.Expression(SynExpr.LetOrUse(_, false, _, _, range)) :: _ -> 
                Some(range)
            | _ -> None

        match bindingRange with
        | Some letBindingRange -> 
            checkForBindingToAWildcard pattern range args.FileContent expr letBindingRange
        | None -> Array.empty
    | _ -> Array.empty

/// Checks if any code uses 'let _ = ...' and suggests to use the ignore function.
let rule =
    { Name = "FavourIgnoreOverLetWild"
      Identifier = Identifiers.FavourIgnoreOverLetWild
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
