module FSharpLint.Rules.RecursiveAsyncFunction

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private isAsyncCompExpr = function
    | SynExpr.App (_, _, (SynExpr.Ident compExprName), (SynExpr.CompExpr _), _)
        when compExprName.idText = "async" ->
            true
    | _ -> false

let rec private getIdentFromSynPat = function
    | SynPat.LongIdent (longDotId=longDotId) ->
        longDotId
        |> ExpressionUtilities.longIdentWithDotsToString
        |> Some
    | SynPat.Typed (pat, _, _) ->
        getIdentFromSynPat pat
    | _ ->
        None

let private getFunctionNameFromAsyncCompExprBinding = function
    | SynBinding.Binding (headPat=headPat; expr=expr) when isAsyncCompExpr expr ->
        getIdentFromSynPat headPat
    | _ ->
        None

let checkRecursiveAsyncFunction (args:AstNodeRuleParams) (range:range) (doBangExpr:SynExpr) breadcrumbs =
    let doTokenRange = mkRange "do!" (mkPos range.StartLine range.StartColumn) (mkPos range.StartLine (range.StartColumn + 3))
    match doBangExpr with
    | SynExpr.App (funcExpr=(SynExpr.Ident callerIdent)) ->
        breadcrumbs
        |> List.collect (fun crumb ->
            match crumb with
            | AstNode.ModuleDeclaration (SynModuleDecl.Let (true, bindings, _)) ->
                bindings
            | AstNode.Expression (SynExpr.LetOrUse (true, false, bindings, _, _)) ->
                bindings
            | _ -> [])
        |> List.choose getFunctionNameFromAsyncCompExprBinding
        |> List.filter ((=) callerIdent.idText)
        |> List.choose (fun _ ->
            let suggestedFix = lazy(
                ExpressionUtilities.tryFindTextOfRange doTokenRange args.fileContent
                |> Option.map (fun fromText ->
                    { FromText = fromText
                      FromRange = doTokenRange
                      ToText = "return!" }))

            { Range = range
              Message = Resources.GetString("RulesConventionsRecursiveAsyncFunctionError")
              SuggestedFix = Some suggestedFix
              TypeChecks = [] } |> Some)
        |> List.toArray
    | _ -> Array.empty
    
let runner args =
    match args.astNode with
    | AstNode.Expression (SynExpr.DoBang (expr, range)) ->
        let parents = args.getParents 5
        checkRecursiveAsyncFunction args range expr parents
    | _ -> Array.empty
    
let rule =
    { name = "RecursiveAsyncFunction" 
      identifier = None
      ruleConfig = { AstNodeRuleConfig.runner = runner } }
    |> AstNodeRule
