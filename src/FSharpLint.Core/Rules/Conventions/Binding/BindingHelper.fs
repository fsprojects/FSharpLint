module FSharpLint.Rules.Helper.Binding

open FSharpLint.Framework
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast

let isLetBinding index (syntaxArray:AbstractSyntaxArray.Node []) =
    if index > 0 then
        match syntaxArray.[syntaxArray.[index].ParentIndex].Actual with
        | AstNode.ModuleDeclaration(SynModuleDecl.Let(_))
        | AstNode.Expression(SynExpr.LetOrUse(_, false, _, false, _, _, _, _)) -> true
        | _ -> false
    else false
