module FSharpLint.Rules.FavourAsKeyword

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private checkForNamedPatternEqualsConstant (args:AstNodeRuleParams) pattern whenExpr (range:Range) =
    let patternIdent =
        match pattern with
        | SynPat.Named(SynIdent.SynIdent(ident, _), _, _, _) -> Some(ident.idText)
        | _ -> None


    match whenExpr with
    | SynExpr.App(_, _, funcExpr, SynExpr.Const(_, constRange), _) ->
        match funcExpr with
        | SynExpr.App(_, _, ExpressionUtilities.Identifier([opIdent], _), SynExpr.Ident(ident), _)
            when opIdent.idText = "op_Equality" && Option.contains ident.idText patternIdent ->

            let fromRange = Range.mkRange "" range.Start constRange.End

            let suggestedFix =
                ExpressionUtilities.tryFindTextOfRange fromRange args.FileContent
                |> Option.bind (fun text ->

                    ExpressionUtilities.tryFindTextOfRange constRange args.FileContent
                    |> Option.bind (fun constText ->

                        lazy (Some { FromText = text; FromRange = fromRange; ToText = $"{constText} as {ident.idText}"})
                        |> Some
                    )

                )

            { Range = fromRange
              Message = Resources.GetString("RulesFavourAsKeyword")
              SuggestedFix = suggestedFix
              TypeChecks = [] } |> Array.singleton

        | _ -> Array.empty
    | _ -> Array.empty

let private runner (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Match(SynMatchClause.SynMatchClause(pat, Some(whenExpr), _, range, _, _)) ->
        checkForNamedPatternEqualsConstant args pat whenExpr range

    | _ -> Array.empty

let rule =
    { Name = "FavourAsKeyword"
      Identifier = Identifiers.FavourAsKeyword
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
