namespace FSharpLint.Rules

module Conventions =

    open FSharpLint.Framework
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Configuration
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast

    [<Literal>]
    let AnalyserName = "Conventions"
    
    let private isAnalyserEnabled config =
        isAnalyserEnabled config AnalyserName |> Option.isSome

    let private isEnabled ruleName config =
        isRuleEnabled config AnalyserName ruleName |> Option.isSome

    module private TopLevelNamespace =

        let checkTopLevelNamespace args range moduleOrNamespace isSuppressed =
            let ruleName = "TopLevelNamespace"

            let isEnabled = isEnabled ruleName args.Info.Config

            if isEnabled && isSuppressed ruleName |> not then
                let (SynModuleOrNamespace(_, _, isModule, _, _, _, _, _)) = moduleOrNamespace
                if isModule
                then
                    args.Info.Suggest
                        { Range = range 
                          Message = Resources.GetString("RulesConventionsTopLevelNamespaceError")
                          SuggestedFix = None
                          TypeChecks = [] }

    module private RecursiveAsyncFunction =

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

        let checkRecursiveAsyncFunction args (range:range) (doBangExpr:SynExpr) breadcrumbs isSuppressed =
            let ruleName = "RecursiveAsyncFunction"

            let isEnabled = isEnabled ruleName args.Info.Config

            if isEnabled && isSuppressed ruleName |> not then
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
                    |> List.iter (fun _ ->
                        let suggestedFix = lazy(
                            args.Info.TryFindTextOfRange doTokenRange
                            |> Option.map (fun fromText -> 
                                { FromText = fromText
                                  FromRange = doTokenRange
                                  ToText = "return!" }))

                        args.Info.Suggest
                            { Range = range 
                              Message = Resources.GetString("RulesConventionsRecursiveAsyncFunctionError")
                              SuggestedFix = Some suggestedFix
                              TypeChecks = [] }
                    )
                | _ -> ()

    let analyser (args: AnalyserArgs) : unit = 
        if isAnalyserEnabled args.Info.Config then
            let syntaxArray, skipArray = args.SyntaxArray, args.SkipArray

            let isSuppressed i ruleName =
                AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
                |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName
                
            syntaxArray
            |> Array.tryHead
            |> Option.iter (fun firstNode ->
                match firstNode.Actual with
                | AstNode.ModuleOrNamespace moduleOrNamespace ->
                    TopLevelNamespace.checkTopLevelNamespace args moduleOrNamespace.Range moduleOrNamespace (isSuppressed 0)
                | _ -> ())

            for i = 0 to syntaxArray.Length - 1 do
                let node = syntaxArray.[i].Actual
                match node with 
                | AstNode.Expression (SynExpr.DoBang (expr, range)) ->
                    let breadcrumbs = AbstractSyntaxArray.getBreadcrumbs 5 syntaxArray skipArray i
                    RecursiveAsyncFunction.checkRecursiveAsyncFunction args range expr breadcrumbs (isSuppressed i)
                | _ -> ()