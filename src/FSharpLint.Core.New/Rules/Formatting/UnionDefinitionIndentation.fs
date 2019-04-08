module FSharpLint.Rules.UnionDefinitionIndentation

open System
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharpLint.Framework
open FSharpLint.Framework.Analyser
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.ExpressionUtilities

let getUnionCaseStartColumn (text:string) (SynUnionCase.UnionCase (attrs, _, _, _, _, range)) =
    match attrs |> List.tryHead with
    | Some attr ->
        let range = mkRange "" (mkPos attr.Range.StartLine 0) attr.Range.Start
        tryFindTextOfRange range text
        |> Option.bind (fun preceedingText ->
            let attrStartIndex = preceedingText.IndexOf "[<"
            if attrStartIndex = -1
            then None
            else Some attrStartIndex)
        |> Option.defaultValue (attr.Range.StartColumn - 2)
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
                if getUnionCaseStartColumn args.fileContent firstCase <> typeDefnStartColumn + 1 then
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
                    if getUnionCaseStartColumn args.fileContent caseOne <> getUnionCaseStartColumn args.fileContent caseTwo then
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
    match args.astNode with
    | AstNode.TypeDefinition (SynTypeDefn.TypeDefn (_, repr, members, defnRange)) ->
        checkUnionDefinitionIndentation args repr defnRange.StartColumn
    | _ ->
        Array.empty
        
let rule =
    { name = "UnionDefinitionIndentation" 
      identifier = None
      ruleConfig = { runner = runner} }
    |> AstNodeRule
