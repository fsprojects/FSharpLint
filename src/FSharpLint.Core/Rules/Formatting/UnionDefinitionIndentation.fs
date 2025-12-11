module FSharpLint.Rules.UnionDefinitionIndentation

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let getUnionCaseStartColumn (SynUnionCase (attrs, _, _, _, _, range, _)) =
    match List.tryHead attrs with
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
            let indentationLevelViolation =
                if getUnionCaseStartColumn firstCase - 2 <> typeDefnStartColumn + args.GlobalConfig.numIndentationSpaces then
                    Some
                        {
                            Range = firstCase.Range
                            Message = Resources.GetString "RulesFormattingUnionDefinitionIndentationViolation"
                            AutoFix = None
                            TypeChecks = List.Empty
                        }
                else
                    None

            let consistentIndentationViolations =
                let choose caseOne caseTwo = 
                    if getUnionCaseStartColumn caseOne <> getUnionCaseStartColumn caseTwo then
                        Some
                            {
                                Range = caseTwo.Range
                                Message = Resources.GetString "RulesFormattingUnionDefinitionSameIndentationViolation"
                                AutoFix = None
                                TypeChecks = List.Empty
                            }
                    else
                        None

                cases
                |> List.toArray
                |> Array.pairwise
                |> Array.choose (fun (caseOne, caseTwo) -> choose caseOne caseTwo)

            Array.concat
                [|
                    Option.toArray indentationLevelViolation
                    consistentIndentationViolations
                |]
    | _ -> Array.empty

let runner args =
    match args.AstNode with
    | AstNode.ModuleDeclaration (SynModuleDecl.Types (typeDefns, typesRange)) ->
        typeDefns
        |> List.toArray
        |> Array.collect (fun (SynTypeDefn (_, repr, _, _, _, _)) ->
            checkUnionDefinitionIndentation args repr typesRange.StartColumn)
    | _ ->
        Array.empty

let rule =
    AstNodeRule
        {
            Name = "UnionDefinitionIndentation"
            Identifier = Identifiers.UnionDefinitionIndentation
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
