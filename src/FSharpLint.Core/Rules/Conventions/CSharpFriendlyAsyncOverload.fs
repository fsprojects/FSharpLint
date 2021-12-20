module FSharpLint.Rules.CSharpFriendlyAsyncOverload

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System

type NodeDetails = { Ident: string; Range: range }

let rec private getIdentFromSynPat =
    function
    | SynPat.LongIdent (longDotId = longDotId) ->
        longDotId
        |> ExpressionUtilities.longIdentWithDotsToString
        |> Some
    | SynPat.Typed (pat, _, _) -> getIdentFromSynPat pat
    | _ -> None

let runner (args: AstNodeRuleParams) =
    let hasAsync (syntaxArray: array<AbstractSyntaxArray.Node>) nodeIndex fnIdent =
        let rec hasAsync index =
            if index >= syntaxArray.Length then
                None
            else
                let node = syntaxArray.[index].Actual
                match node with
                | AstNode.Binding (SynBinding (_, _, _, _, _attributes, _, _, pattern, _, _, range, _)) ->
                    match getIdentFromSynPat pattern with
                    | Some ident when ident = fnIdent + "Async" ->
                        { Ident = fnIdent
                          Range = range } |> Some
                    | _ -> hasAsync (index + 1)
                | _ -> hasAsync (index + 1)

        hasAsync nodeIndex

    match args.AstNode with
    | AstNode.Binding (SynBinding (_, _, _, _, _, _, _, pattern, synInfo, _, range, _)) ->
        match synInfo with
        | Some (SynBindingReturnInfo (SynType.App(SynType.LongIdent(LongIdentWithDots(ident, _)), _, _, _, _, _, _), _, _)) ->
            match ident with
            | head::_ when head.idText = "Async" ->
                let idents = getIdentFromSynPat pattern
                match idents with
                | Some ident when not (ident.EndsWith "Async") ->
                    match hasAsync args.SyntaxArray args.NodeIndex ident with
                    | Some _ -> Array.empty
                    | None ->
                        { Range = range
                          Message = String.Format(Resources.GetString "RulesCSharpFriendlyAsyncOverload", ident)
                          SuggestedFix = None
                          TypeChecks = List.Empty }
                        |> Array.singleton
                | _ -> Array.empty
            | _ -> Array.empty
        | _ -> Array.empty
    | _ -> Array.empty


let rule =
    { Name = "CSharpFriendlyAsyncOverload"
      Identifier = Identifiers.CSharpFriendlyAsyncOverload
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
