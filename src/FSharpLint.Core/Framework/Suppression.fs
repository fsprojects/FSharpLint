module FSharpLint.Framework.Suppression

open System.Text.RegularExpressions

type SuppressionTarget =
    | All
    | Rules of rules : Set<string>

/// Represents suppression information found in a comment.
type SuppressionInfo =
    /// Represents a comment disabling linting rules for the rest of the file.
    | Disable of SuppressionTarget
    /// Represents a comment enabling linting rules for the rest of the file.
    | Enable of SuppressionTarget
    /// Represents a comment disabling linting rules for the next line.
    | DisableNextLine of SuppressionTarget
    /// Represents a comment enabling linting rules for the next line.
    | EnableNextLine of SuppressionTarget

let parseSuppressionInfo (lines : (string * int) []) =
    lines
    |> Array.map (fun (line, lineNum) ->
        let trimmedLine = line.TrimStart()
        let matched = Regex.Match (trimmedLine, "^\/\/\s*fsharplint:([a-z\-]*)\s*(.*)$", RegexOptions.IgnoreCase)
        if matched.Success then
            let suppressionTarget =
                if matched.Groups.Count = 3 then
                    let target = matched.Groups.[2].Value
                    if target = "" then
                        All
                    else
                        Rules (target.Split([|' '|]) |> Set.ofArray)
                else
                    All
            match matched.Groups.[1].Value with
            | "enable" -> (lineNum, Some (Enable suppressionTarget))
            | "disable" -> (lineNum, Some (Disable suppressionTarget))
            | "enable-next-line" -> (lineNum, Some (EnableNextLine suppressionTarget))
            | "disable-next-line" -> (lineNum, Some (DisableNextLine suppressionTarget))
            | _ ->
                // TODO: print warning
                (lineNum, None)
        else
            (lineNum, None))

type NextLineSuppression =
    | EnableLine of SuppressionTarget
    | DisableLine of SuppressionTarget

let getCurrentLineSuppressedRules (allRules : Set<string>) (currentSuppressedRules:Set<string>, nextLineSuppressions:NextLineSuppression option) =
    match nextLineSuppressions with
    | Some (EnableLine All) ->
        Set.empty
    | Some (DisableLine All) ->
        allRules
    | Some (EnableLine (Rules rules)) ->
        Set.difference currentSuppressedRules rules
    | Some (DisableLine (Rules rules)) ->
        Set.union currentSuppressedRules rules
    | None ->
        currentSuppressedRules

let getNextLineSuppressionContext (allRules : Set<string>) (currentSuppressedRules : Set<string>) (suppressionInfo : SuppressionInfo option) =
    match suppressionInfo with
    | Some (Enable All) ->
        (Set.empty, None)
    | Some (Enable (Rules rules)) ->
        (Set.difference currentSuppressedRules rules, None)
    | Some (Disable All) ->
        (allRules, None)
    | Some (Disable (Rules rules)) ->
        (Set.union currentSuppressedRules rules, None)
    | Some (EnableNextLine target) ->
         (currentSuppressedRules, Some (NextLineSuppression.EnableLine target))
    | Some (DisableNextLine target) ->
        (currentSuppressedRules, Some (NextLineSuppression.DisableLine target))
    | None -> (currentSuppressedRules, None)

let getSuppressedRulesPerLine (allRules : Set<string>) (lines : (string * int) []) =
    parseSuppressionInfo lines
    |> Array.fold (fun ((currentSuppressedRules, nextLineSuppressions:NextLineSuppression option), agg) (lineNum, suppressionInfo) ->
        let currentLineSuppressions = getCurrentLineSuppressedRules allRules (currentSuppressedRules, nextLineSuppressions)
        let (nextLineSuppressedRules, nextLineSuppression) = getNextLineSuppressionContext allRules currentSuppressedRules suppressionInfo
        ((nextLineSuppressedRules, nextLineSuppression), (lineNum, currentLineSuppressions) :: agg)) ((Set.empty, None), [])
    |> snd
    |> dict
