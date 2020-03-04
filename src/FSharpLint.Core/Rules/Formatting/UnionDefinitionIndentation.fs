module FSharpLint.Rules.UnionDefinitionIndentation

open System
open FSharp.Compiler.Ast
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let getUnionCaseStartColumn (SynUnionCase.UnionCase (attrs, _, _, _, _, range)) =
    match attrs |> List.tryHead with
    | Some attr ->
        // startcolumn of the attributes now includes the `[<` starter sigil, so we can just use it!
        attr.Range.StartColumn
    | None ->
        range.StartColumn

let checkUnionDefinitionIndentation (args:AstNodeRuleParams) typeDefnRepr typeDefnStartColumn =
    match typeDefnRepr with
    | SynTypeDefnRepr.Simple((SynTypeDefnSimpleRepr.Union (_, cases, _)), _) ->
        match cases with
        | []
        | [_] -> Array.empty
        | firstCase :: _ ->
            let indentationLevelError =
                if getUnionCaseStartColumn firstCase - 2 <> typeDefnStartColumn + args.GlobalConfig.numIndentationSpaces then
                    { Range = firstCase.Range
                      Message = Resources.GetString("RulesFormattingUnionDefinitionIndentationError")
                      SuggestedFix = None
                      TypeChecks = [] } |> Some
                else
                    None

            let consistentIndentationErrors =
                cases
                |> List.toArray
                |> Array.pairwise
                |> Array.choose (fun (caseOne, caseTwo) ->
                    if getUnionCaseStartColumn caseOne <> getUnionCaseStartColumn caseTwo then
                        { Range = caseTwo.Range
                          Message = Resources.GetString("RulesFormattingUnionDefinitionSameIndentationError")
                          SuggestedFix = None
                          TypeChecks = [] } |> Some
                    else
                        None)

            [|
                indentationLevelError |> Option.toArray
                consistentIndentationErrors
            |]
            |> Array.concat
    | _ -> Array.empty

let runner args =
    match args.AstNode with
    | AstNode.ModuleDeclaration (SynModuleDecl.Types (typeDefns, typesRange)) ->
        typeDefns
        |> List.toArray
        |> Array.collect (fun (SynTypeDefn.TypeDefn (_, repr, _, _)) ->
            checkUnionDefinitionIndentation args repr typesRange.StartColumn)
    | _ ->
        Array.empty

let rule =
    { Name = "UnionDefinitionIndentation"
      Identifier = Identifiers.UnionDefinitionIndentation
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
