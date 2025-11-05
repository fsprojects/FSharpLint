module FSharpLint.Rules.NoAsyncRunSynchronouslyInLibrary

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Utilities

let extractAttributeNames (attributes: SynAttributes) =
    seq {
        for attr in extractAttributes attributes do
            match attr.TypeName with
            | SynLongIdent([ident], _, _) -> yield ident.idText
            | _ -> ()
    }

let hasEntryPointAttribute (syntaxArray: array<AbstractSyntaxArray.Node>) =
    syntaxArray
    |> Array.exists 
        (fun node -> 
            match node.Actual with
            | AstNode.Binding(SynBinding(_, _, _, _, attributes, _, _, _, _, _, _, _, _)) -> 
                attributes 
                |> extractAttributeNames
                |> Seq.contains "EntryPoint"
            | _ -> false)

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
    let isInTestProject =
        match args.CheckInfo with
        | Some checkFileResults -> 
            let namespaceIncludesTest =
                match checkFileResults.ImplementationFile with
                | Some implFile -> implFile.QualifiedName.ToLowerInvariant().Contains "test"
                | None -> false
            let projectFileInfo = System.IO.FileInfo checkFileResults.ProjectContext.ProjectOptions.ProjectFileName
            namespaceIncludesTest || projectFileInfo.Name.ToLowerInvariant().Contains "test"
        | None -> false
    
    if isInTestProject || isInsideTest (args.GetParents args.NodeIndex) || hasEntryPointAttribute args.SyntaxArray then
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
