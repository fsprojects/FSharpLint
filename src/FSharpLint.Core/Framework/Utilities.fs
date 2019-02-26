namespace FSharpLint.Framework

module Utilities =

    /// Fast hash of two objects without having to allocate
    /// (e.g. a common approach would be to take the hash code of a tuple of the two objects).
    let inline hash2 one two =
        let mutable current = 23
        current <- current * 31 + hash one
        current * 31 + hash two

    let (</>) x y = System.IO.Path.Combine(x, y)

module Dictionary =

    open System.Collections.Generic

    let addOrUpdate key value (dict:Dictionary<'key,'value>) =
        if dict.ContainsKey(key) then
            dict.Remove(key) |> ignore

        dict.Add(key, value)

module ExpressionUtilities =

    open FSharp.Compiler
    open FSharp.Compiler.Ast
    open FSharp.Compiler.Range
    open FSharp.Compiler.SourceCodeServices

    let (|Identifier|_|) = function
        | SynExpr.Ident(ident) -> Some([ident], ident.idRange)
        | SynExpr.LongIdent(_, longIdent, _, _) -> Some(longIdent.Lid, longIdent.Range)
        | _ -> None

    let getSymbolFromIdent (checkFile:FSharpCheckFileResults option) expr =
        match checkFile, expr with
        | Some(checkFile), Identifier(ident, range) ->
            let identNames = ident |> List.map (fun x -> x.idText)

            checkFile.GetSymbolUseAtLocation(
                range.StartLine,
                range.EndColumn,
                "",
                identNames) |> Async.RunSynchronously
        | _ -> None

    /// Converts an operator name e.g. op_Add to the operator symbol e.g. +
    let identAsDecompiledOpName (ident:Ident) =
        if ident.idText.StartsWith("op_") then
            PrettyNaming.DecompileOpName ident.idText
        else ident.idText

    let identAsCompiledOpName = PrettyNaming.CompileOpName

    /// Extracts an expression from parentheses e.g. ((x + 4)) -> x + 4
    let rec removeParens = function
        | SynExpr.Paren(x, _, _, _) -> removeParens x
        | x -> x

    /// Finds index of a given (line number, column) position in a string.
    let findPos (pos:pos) (str:string) =
        let rec findLineStart (lineNumber:int) currLine (currPos:int) =
            if currLine = lineNumber then Some currPos
            else
                let nextLinePos = str.IndexOf('\n', currPos)
                if nextLinePos >= 0 then findLineStart lineNumber (currLine + 1) (nextLinePos + 1)
                else None

        findLineStart pos.Line 1 0
        |> Option.map (fun x -> x + pos.Column)

    /// Converts a LongIdent to a String.
    let longIdentToString (lid:LongIdent) =
        lid |> List.map (fun li -> li.idText) |> String.concat "."

    /// Converts a LongIdentWithDots to a String.
    let longIdentWithDotsToString (lidwd:LongIdentWithDots) =
        lidwd.Lid |> longIdentToString
