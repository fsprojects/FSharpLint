module FSharpLint.Rules.Utilities

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
