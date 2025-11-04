module FSharpLint.Rules.NoAsyncRunSynchronouslyInLibrary

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Utilities

let hasEntryPointAttribute (syntaxArray: array<AbstractSyntaxArray.Node>) =
    let hasEntryPoint (attrs: SynAttributeList) =
        attrs.Attributes 
        |> List.exists 
            (fun attr -> 
                match attr.TypeName with
                | SynLongIdent([ident], _, _) -> ident.idText = "EntryPoint"
                | _ -> false)

    syntaxArray
    |> Array.exists 
        (fun node -> 
            match node.Actual with
            | AstNode.Binding(SynBinding(_, _, _, _, attributes, _, _, _, _, _, _, _, _)) -> 
                attributes |> List.exists hasEntryPoint
            | _ -> false)

let checkIfInLibrary (syntaxArray: array<AbstractSyntaxArray.Node>) (checkInfo: option<FSharpCheckFileResults>) (range: range) : array<WarningDetails> =
    let isInTestAssembly =
        match checkInfo with
        | Some checkFileResults -> 
            match Seq.tryHead checkFileResults.PartialAssemblySignature.Entities with
            | Some entity -> entity.Assembly.QualifiedName.ToLowerInvariant().Contains "test"
            | None -> false
        | None -> false
    
    if isInTestAssembly || hasEntryPointAttribute syntaxArray then
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
        checkIfInLibrary args.SyntaxArray args.CheckInfo range
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
