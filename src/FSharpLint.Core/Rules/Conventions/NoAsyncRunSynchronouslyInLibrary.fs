module FSharpLint.Rules.NoAsyncRunSynchronouslyInLibrary

open FSharp.Compiler.Syntax
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Utilities

let hasEntryPoint (checkFileResults: FSharpCheckFileResults) (maybeProjectCheckResults: FSharpCheckProjectResults option) =
    let hasEntryPointInTheSameFile =
        match checkFileResults.ImplementationFile with
        | Some implFile -> implFile.HasExplicitEntryPoint
        | None -> false

    hasEntryPointInTheSameFile
    ||
    match maybeProjectCheckResults with
    | Some projectCheckResults ->
        projectCheckResults.AssemblyContents.ImplementationFiles
        |> Seq.exists (fun implFile -> implFile.HasExplicitEntryPoint)
    | None -> false

let excludedProjectNames = [ "test"; "console" ]

let isInTestProject (checkFileResults: FSharpCheckFileResults) =
    let namespaceIncludesTest =
        match checkFileResults.ImplementationFile with
        | Some implFile -> 
            excludedProjectNames |> List.exists (fun name -> implFile.QualifiedName.ToLowerInvariant().Contains name)
        | None -> false
    let projectFileInfo = System.IO.FileInfo checkFileResults.ProjectContext.ProjectOptions.ProjectFileName
    namespaceIncludesTest 
    || excludedProjectNames |> List.exists (fun name -> projectFileInfo.Name.ToLowerInvariant().Contains name)

let extractAttributeNames (attributes: SynAttributes) =
    seq {
        for attr in extractAttributes attributes do
            match attr.TypeName with
            | SynLongIdent([ident], _, _) -> yield ident.idText
            | _ -> ()
    }

let testMethodAttributes = [ "Test"; "TestMethod" ]
let testClassAttributes = [ "TestFixture"; "TestClass" ]

let isInTheSameModuleAsTest (nodes: array<AbstractSyntaxArray.Node>) (maybeProjectCheckResults: FSharpCheckProjectResults option) =
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
    
    let isDeclarationOfTestClass declaration =
        match declaration with
        | FSharpImplementationFileDeclaration.Entity(entity, _) ->
            entity.Attributes
            |> Seq.exists (fun attr -> testClassAttributes |> List.contains attr.AttributeType.DisplayName)
        | _ -> false

    match maybeProjectCheckResults with
    | Some projectCheckResults ->
        projectCheckResults.AssemblyContents.ImplementationFiles
        |> Seq.exists (fun implFile -> 
            implFile.Declarations
            |> Seq.exists isDeclarationOfTestClass
        )
    | None ->
        nodes |> Array.exists (fun node -> isTestMethodOrClass node.Actual)

let checkIfInLibrary (args: AstNodeRuleParams) (range: range) : array<WarningDetails> =
    let ruleNotApplicable =
        match args.CheckInfo with
        | Some checkFileResults ->
            hasEntryPoint checkFileResults args.ProjectCheckInfo
            || isInTestProject checkFileResults
            || isInTheSameModuleAsTest args.SyntaxArray args.ProjectCheckInfo
        | None ->
            isInTheSameModuleAsTest args.SyntaxArray args.ProjectCheckInfo
    
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
