module FSharpLint.Rules.Helper.Binding

open FSharpLint.Framework
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast

let isLetBinding i (syntaxArray:AbstractSyntaxArray.Node []) =
    if i > 0 then
        match syntaxArray.[syntaxArray.[i].ParentIndex].Actual with
        | AstNode.ModuleDeclaration(SynModuleDecl.Let(_))
        | AstNode.Expression(SynExpr.LetOrUse(_, false, _, _, _)) -> true
        | _ -> false
    else false
