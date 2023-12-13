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

    let addOrUpdate key value (dict:Dictionary<'Key,'Value>) =
        if dict.ContainsKey(key) then
            dict.Remove(key) |> ignore

        dict.Add(key, value)

module ExpressionUtilities =

    open System
    open FSharp.Compiler
    open FSharp.Compiler.Syntax
    open FSharp.Compiler.Text
    open FSharp.Compiler.CodeAnalysis

    let (|Identifier|_|) = function
        | SynExpr.Ident(ident) -> Some([ident], ident.idRange)
        | SynExpr.LongIdent(_, longIdent, _, _) -> Some(longIdent.Lid, longIdent.Range)
        | _ -> None

    let getSymbolFromIdent (checkFile:FSharpCheckFileResults option) expr =
        match (checkFile, expr) with
        | Some(checkFile), Identifier(ident, range) ->
            let identNames = ident |> List.map (fun x -> x.idText)

            checkFile.GetSymbolUseAtLocation(
                range.StartLine,
                range.EndColumn,
                "",
                identNames)
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

    /// Tries to find the source code within a given range.
    let tryFindTextOfRange (range:Range) (text:string) =
        let startIndex = findPos range.Start text
        let endIndex = findPos range.End text

        match (startIndex, endIndex) with
        | Some(startIndex), Some(endIndex) ->
            text.Substring(startIndex, endIndex - startIndex) |> Some
        | _ -> None

    let getLeadingSpaces (range:Range) (text:string) =
        let range = Range.mkRange "" (Position.mkPos range.StartLine 0) range.End
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
            |> Option.map (fun x -> "(" + x + ")")
        | other ->
            tryFindTextOfRange other.Range text

    /// Converts a list of type args to its string representation.
    let typeArgsToString (text:string) (typeArgs:SynType list) =
        let typeStrings = typeArgs |> List.choose (synTypeToString text)
        if typeStrings.Length = typeArgs.Length
        then typeStrings |> String.concat "," |> Some
        else None

    /// Counts the number of comment lines preceding the given range of text.
    let countPrecedingCommentLines (text:string) (startPos:pos) (endPos:pos) =
        let range = Range.mkRange "" startPos endPos

        tryFindTextOfRange range text
        |> Option.map (fun precedingText ->
            let lines =
                precedingText.Split '\n'
                |> Array.rev
                |> Array.tail
            lines
            |> Array.takeWhile (fun line -> line.TrimStart().StartsWith("//"))
            |> Array.length)
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

        let rec iterateLines currentLine i =
            match currentLine with
            | Some line ->
                let nextLine = readLine ()
                let isLastLine = Option.isNone nextLine

                lines.Add(line, i, isLastLine)

                iterateLines nextLine (i + 1)
            | None -> ()

        iterateLines (readLine ()) 0

        lines.ToArray()