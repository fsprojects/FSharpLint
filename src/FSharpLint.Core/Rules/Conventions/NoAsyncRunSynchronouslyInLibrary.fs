module FSharpLint.Rules.NoAsyncRunSynchronouslyInLibrary

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Utilities

let hasEntryPoint (checkFileResults: FSharpCheckFileResults) =
    match checkFileResults.ImplementationFile with
    | Some implFile -> implFile.HasExplicitEntryPoint
    | None -> false

let isInTestProject (checkFileResults: FSharpCheckFileResults) =
    let namespaceIncludesTest =
        match checkFileResults.ImplementationFile with
        | Some implFile -> implFile.QualifiedName.ToLowerInvariant().Contains "test"
        | None -> false
    let projectFileInfo = System.IO.FileInfo checkFileResults.ProjectContext.ProjectOptions.ProjectFileName
    namespaceIncludesTest || projectFileInfo.Name.ToLowerInvariant().Contains "test"

let extractAttributeNames (attributes: SynAttributes) =
    seq {
        for attr in extractAttributes attributes do
            match attr.TypeName with
            | SynLongIdent([ident], _, _) -> yield ident.idText
            | _ -> ()
    }

let testMethodAttributes = [ "Test"; "TestMethod" ]
let testClassAttributes = [ "TestFixture"; "TestClass" ]

let isInsideTest (parents: list<AstNode>)  =
    let isTestMethodOrClass node =
        match node with
        | AstNode.MemberDefinition(SynMemberDefn.Member(SynBinding(_, _, _, _, attributes, _, _, _, _, _, _, _, _), _)) ->
            attributes 
            |> extractAttributeNames
            |> Seq.exists (fun name -> testMethodAttributes |> List.contains name)
        | AstNode.TypeDefinition(SynTypeDefn.SynTypeDefn(SynComponentInfo(attributes, _, _, _, _, _, _, _), _, _, _, _, _)) ->
            attributes 
            |> extractAttributeNames
            |> Seq.exists (fun name -> testClassAttributes |> List.contains name)
        | _ -> false
    
    parents |> List.exists isTestMethodOrClass

let checkIfInLibrary (args: AstNodeRuleParams) (range: range) : array<WarningDetails> =
    let ruleNotApplicable =
        match args.CheckInfo with
        | Some checkFileResults ->
            hasEntryPoint checkFileResults || isInTestProject checkFileResults || isInsideTest (args.GetParents args.NodeIndex)
        | None ->
            isInsideTest (args.GetParents args.NodeIndex)
    
    if ruleNotApplicable then
        Array.empty
    else
        Array.singleton 
            { 
                Range = range
                Message = Resources.GetString "NoAsyncRunSynchronouslyInLibrary"
                SuggestedFix = None
                TypeChecks = List.Empty 
            }

let runner args =
    match args.AstNode with
    | AstNode.Identifier(["Async"; "RunSynchronously"], range) ->
        checkIfInLibrary args range
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "NoAsyncRunSynchronouslyInLibrary"
            Identifier = Identifiers.NoAsyncRunSynchronouslyInLibrary
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
