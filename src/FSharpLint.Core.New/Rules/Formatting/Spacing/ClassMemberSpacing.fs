module FSharpLint.Rules.ClassMemberSpacing

open System
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharpLint.Framework
open FSharpLint.Framework.Analyser
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.ExpressionUtilities

let checkClassMemberSpacing (args:AstNodeRuleParams) (members:SynMemberDefns) =
    members
    |> List.toArray
    |> Array.pairwise
    |> Array.choose (fun (memberOne, memberTwo) ->
        let numPreceedingCommentLines = countPrecedingCommentLines args.fileContent memberOne.Range.End memberTwo.Range.Start
        if memberTwo.Range.StartLine <> memberOne.Range.EndLine + 2 + numPreceedingCommentLines then
            let intermediateRange =
                let startLine = memberOne.Range.EndLine + 1
                let endLine = memberTwo.Range.StartLine
                let endOffset =
                    if startLine = endLine
                    then 1
                    else 0

                mkRange
                    ""
                    (mkPos (memberOne.Range.EndLine + 1) 0)
                    (mkPos (memberTwo.Range.StartLine + endOffset) 0)
            
            { Range = intermediateRange
              Message = Resources.GetString("RulesFormattingClassMemberSpacingError")
              SuggestedFix = None
              TypeChecks = [] } |> Some
        else
            None)
        
let runner args =
    match args.astNode with 
    | AstNode.TypeDefinition (SynTypeDefn.TypeDefn (_, repr, members, defnRange)) ->
        checkClassMemberSpacing args members
    | AstNode.TypeRepresentation (SynTypeDefnRepr.ObjectModel (_, members, _)) ->
        checkClassMemberSpacing args members
    | _ -> Array.empty
        
let rule =
    { name = "ClassMemberSpacing" 
      identifier = None
      ruleConfig = { AstNodeRuleConfig.runner = runner } }
    |> AstNodeRule