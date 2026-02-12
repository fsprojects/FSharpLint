module FSharpLint.Rules.Utilities

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax

module TypedTree =
    let tryGetLastGenericArg (fSharpType: FSharpType) =
        Seq.tryLast fSharpType.GenericArguments

    [<TailCall>]
    let rec private getReturnType (fSharpType: FSharpType) =
        if fSharpType.IsFunctionType then
            match tryGetLastGenericArg fSharpType with
            | Some argType -> getReturnType argType
            | None -> fSharpType
        else
            fSharpType

    let getFunctionReturnType
        (checkInfo: FSharpCheckFileResults)
        (lines: array<string>)
        (funcIdent: SynLongIdent) : Option<FSharpType> =
        let maybeSymbolUse =
            match List.tryLast funcIdent.LongIdent with
            | Some ident ->
                checkInfo.GetSymbolUseAtLocation(
                    ident.idRange.EndLine,
                    ident.idRange.EndColumn,
                    lines.[ident.idRange.EndLine - 1],
                    List.singleton ident.idText)
            | None -> None
        match maybeSymbolUse with
        | Some symbolUse ->
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as func when func.IsFunction ->
                Some <| getReturnType func.FullType
            | _ -> None
        | _ -> None

module LibraryHeuristics =
    type LibraryHeuristicResultByProjectName =
        | Likely
        | Unlikely
        | Uncertain

    let private projectNamesUnlikelyToBeLibraries =
        [
            "tests"
            "test"
            "testing"
            "console"
            "CLI"
            "TUI"
        ]
        |> Seq.map (fun name -> name.ToLowerInvariant())

    let private possibleProjectNameSegmentSeparators =
        [|
            '.'
            '_'
            '-'
        |]

    let howLikelyProjectIsLibrary (projectFileName: string): LibraryHeuristicResultByProjectName =
        let libraryAbbrev = "lib"
        let nameSegments =
            Helper.Naming.QuickFixes.splitByCaseChange projectFileName
            |> Seq.map (fun segment -> segment.ToLowerInvariant())
        if nameSegments |> Seq.contains libraryAbbrev then
            Likely
        elif
            nameSegments
            |> Seq.exists (
                fun segment ->
                    let subSegments = segment.Split possibleProjectNameSegmentSeparators
                    subSegments
                    |> Seq.exists (fun subSegment ->
                        projectNamesUnlikelyToBeLibraries
                        |> Seq.exists (fun noLibName -> noLibName = subSegment)
                    )
            ) then
            Unlikely
        elif projectFileName.ToLowerInvariant().EndsWith libraryAbbrev then
            Likely
        else
            Uncertain
