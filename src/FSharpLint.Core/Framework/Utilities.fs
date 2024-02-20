﻿namespace FSharpLint.Framework

module Utilities =

    /// Fast hash of two objects without having to allocate
    /// (e.g. a common approach would be to take the hash code of a tuple of the two objects).
    let inline hash2 one two =
        let mutable current = 23
        current <- current * 31 + hash one
        current * 31 + hash two

    let (</>) path1 path2 = System.IO.Path.Combine(path1, path2)

module Dictionary =

    open System.Collections.Generic

    let addOrUpdate key value (dict:Dictionary<'Key,'Value>) =
        if dict.ContainsKey(key) then
            dict.Remove(key) |> ignore<bool>

        dict.Add(key, value)

module ExpressionUtilities =

    open System
    open FSharp.Compiler
    open FSharp.Compiler.Syntax
    open FSharp.Compiler.Text
    open FSharp.Compiler.CodeAnalysis

    open FSharpx.Collections

    let (|Identifier|_|) = function
        | SynExpr.Ident(ident) -> Some([ident], ident.idRange)
        | SynExpr.LongIdent(_, longIdent, _, _) -> Some(longIdent.LongIdent, longIdent.Range)
        | _ -> None

    let getSymbolFromIdent (checkFile:FSharpCheckFileResults option) expr =
        match (checkFile, expr) with
        | Some(checkFile), Identifier(ident, range) ->
            let identNames = List.map (fun (identifier: Ident) -> identifier.idText) ident

            checkFile.GetSymbolUseAtLocation(
                range.StartLine,
                range.EndColumn,
                String.Empty,
                identNames)
        | _ -> None

    /// Converts an operator name e.g. op_Add to the operator symbol e.g. +
    let identAsDecompiledOpName (ident:Ident) =
        if ident.idText.StartsWith("op_") then
            PrettyNaming.DecompileOpName ident.idText
        else ident.idText

    let identAsCompiledOpName (identName: string) = 
        if PrettyNaming.IsOperatorDisplayName identName then
            PrettyNaming.CompileOpName identName
        else
            identName

    /// Extracts an expression from parentheses e.g. ((x + 4)) -> x + 4
    let rec removeParens = function
        | SynExpr.Paren(expr, _, _, _) -> removeParens expr
        | expression -> expression

    /// Finds index of a given (line number, column) position in a string.
    let findPos (pos:pos) (str:string) =
        let rec findLineStart (lineNumber:int) currLine (currPos:int) =
            if currLine = lineNumber then Some currPos
            else
                let nextLinePos = str.IndexOf('\n', currPos)
                if nextLinePos >= 0 then findLineStart lineNumber (currLine + 1) (nextLinePos + 1)
                else None

        findLineStart pos.Line 1 0
        |> Option.map (fun lineStart -> lineStart + pos.Column)

    /// Converts a LongIdent to a String.
    let longIdentToString (lid:LongIdent) =
        lid |> List.map (fun li -> li.idText) |> String.concat "."

    /// Converts a LongIdentWithDots to a String.
    let longIdentWithDotsToString (lidwd: SynLongIdent) =
        longIdentToString lidwd.LongIdent

    /// Tries to find the source code within a given range.
    let tryFindTextOfRange (range:Range) (text:string) =
        let startIndex = findPos range.Start text
        let endIndex = findPos range.End text

        match (startIndex, endIndex) with
        | Some(startIndex), Some(endIndex) ->
            text.Substring(startIndex, endIndex - startIndex) |> Some
        | _ -> None

    let getLeadingSpaces (range:Range) (text:string) =
        let range = Range.mkRange String.Empty (Position.mkPos range.StartLine 0) range.End
        tryFindTextOfRange range text
        |> Option.map (fun text ->
            text.ToCharArray()
            |> Array.takeWhile Char.IsWhiteSpace
            |> Array.length)
        |> Option.defaultValue 0

    /// Converts a SynType to its string representation.
    let synTypeToString (text:string) = function
        | SynType.Tuple _ as synType ->
            tryFindTextOfRange synType.Range text
            |> Option.map (fun extractedText -> "(" + extractedText + ")")
        | other ->
            tryFindTextOfRange other.Range text

    /// Converts a list of type args to its string representation.
    let typeArgsToString (text:string) (typeArgs:SynType list) =
        let typeStrings = List.choose (synTypeToString text) typeArgs
        if typeStrings.Length = typeArgs.Length
        then typeStrings |> String.concat "," |> Some
        else None

    /// Counts the number of comment lines preceding the given range of text.
    let countPrecedingCommentLines (text:string) (startPos:pos) (endPos:pos) =
        let range = Range.mkRange String.Empty startPos endPos

        let processComments (text:string) =
            let lines =
                text.Split '\n'
                |> Seq.rev
            match Seq.tryHeadTail lines with
            | Some (_, tail) ->
                tail
                |> Seq.takeWhile (fun line -> line.TrimStart().StartsWith("//"))
                |> Seq.length
            | None -> 0

        tryFindTextOfRange range text
        |> Option.map processComments
        |> Option.defaultValue 0

    let rangeContainsOtherRange (containingRange:Range) (range:Range) =
        range.StartLine >= containingRange.StartLine && range.EndLine <= containingRange.EndLine

module String =

    open System.IO

    let toLines input =
        let lines = ResizeArray()
        use reader = new StringReader(input)

        let readLine () =
            match reader.ReadLine() with
            | null -> None
            | line -> Some line

        let rec iterateLines currentLine index =
            match currentLine with
            | Some line ->
                let nextLine = readLine ()
                let isLastLine = Option.isNone nextLine

                lines.Add(line, index, isLastLine)

                iterateLines nextLine (index + 1)
            | None -> ()

        iterateLines (readLine ()) 0

        lines.ToArray()