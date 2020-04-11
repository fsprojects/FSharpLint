module FSharpLint.Rules.Helper.Binding

open FSharpLint.Framework
open FSharp.Compiler.SyntaxTree
open FSharpLint.Framework.Ast

let isLetBinding i (syntaxArray:AbstractSyntaxArray.Node []) (skipArray:AbstractSyntaxArray.Skip []) =
    if i > 0 then
        match syntaxArray.[skipArray.[i].ParentIndex].Actual with
        | AstNode.ModuleDeclaration(SynModuleDecl.Let(_))
        | AstNode.Expression(SynExpr.LetOrUse(_, false, _, _, _)) -> true
        | _ -> false
    else false
 