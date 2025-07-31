module FSharpLint.Rules.ClassMemberSpacing

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.ExpressionUtilities

let checkClassMemberSpacing (args:AstNodeRuleParams) (members:SynMemberDefns) =
    let choose (memberOne: SynMemberDefn) (memberTwo: SynMemberDefn) =
        let numPrecedingCommentLines = countPrecedingCommentLines args.FileContent memberOne.Range.End memberTwo.Range.Start
        if memberTwo.Range.StartLine <> memberOne.Range.EndLine + 2 + numPrecedingCommentLines then
            let intermediateRange =
                let startLine = memberOne.Range.EndLine + 1
                let endLine = memberTwo.Range.StartLine
                let endOffset =
                    if startLine = endLine
                    then 1
                    else 0

                Range.mkRange
                    String.Empty
                    (Position.mkPos (memberOne.Range.EndLine + 1) 0)
                    (Position.mkPos (memberTwo.Range.StartLine + endOffset) 0)

            Some
                {
                    Range = intermediateRange
                    Message = Resources.GetString("RulesFormattingClassMemberSpacingError")
                    SuggestedFix = None
                    TypeChecks = List.Empty
                }
        else
            None

    members
    |> List.toArray
    |> Array.pairwise
    |> Array.choose (fun (memberOne, memberTwo) -> choose memberOne memberTwo)

let runner args =
    match args.AstNode with
    | AstNode.TypeDefinition (SynTypeDefn.SynTypeDefn (_, repr, members, implicitCtor, defnRange, _)) ->
        checkClassMemberSpacing args (Option.toList implicitCtor @ members)
    | AstNode.TypeRepresentation (SynTypeDefnRepr.ObjectModel (_, members, _)) ->
        checkClassMemberSpacing args members
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "ClassMemberSpacing"
            Identifier = Identifiers.ClassMemberSpacing
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
