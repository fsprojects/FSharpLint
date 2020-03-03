module FSharpLint.Framework.Suppression

open System.Text.RegularExpressions

type SuppressionTarget =
    | All
    | Rules of rules : Set<string>

/// Represents suppression information found in a comment.
type SuppressionInfo =
    /// Represents a comment enabling linting rules for the rest of the file.
    | Enable of SuppressionTarget
    /// Represents a comment disabling linting rules for the rest of the file.
    | Disable of SuppressionTarget
    /// Represents a comment enabling linting rules for the current line.
    | EnableLine of SuppressionTarget
    /// Represents a comment disabling linting rules for the current line.
    | DisableLine of SuppressionTarget
    /// Represents a comment enabling linting rules for the next line.
    | EnableNextLine of SuppressionTarget
    /// Represents a comment disabling linting rules for the next line.
    | DisableNextLine of SuppressionTarget

let parseSuppressionInfo (lines:string list) =
    ParseFile.tokenizeLines lines
    |> List.map (fun (lineNum, tokens) ->
        ParseFile.collectLineComments tokens
        |> List.tryHead // We just look at the first comment on the line.
        |> Option.map (fun comment ->
            let matched = Regex.Match (comment, ".*fsharplint:([a-z\-]*)\s*(.*)$", RegexOptions.IgnoreCase)
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
                | "enable-line" -> (lineNum, Some (EnableLine suppressionTarget))
                | "disable-line" -> (lineNum, Some (DisableLine suppressionTarget))
                | "enable-next-line" -> (lineNum, Some (EnableNextLine suppressionTarget))
                | "disable-next-line" -> (lineNum, Some (DisableNextLine suppressionTarget))
                | _ ->
                    // TODO: print warning
                    (lineNum, None)
            else
                (lineNum, None))
        |> Option.defaultValue (lineNum, None))

type private LineSuppression =
    | EnableLine of SuppressionTarget
    | DisableLine of SuppressionTarget

/// Gets rules suppressed for the current line.
let private getCurrentLineSuppressedRules (allRules:Set<string>) (currentSuppressedRules:Set<string>, currentLineSuppression:LineSuppression option, nextLineSuppression:LineSuppression option) =
    match (currentLineSuppression, nextLineSuppression) with
    // Current line suppression overrides next line suppressions.
    | (Some (EnableLine All), _) ->
        Set.empty
    | (Some (DisableLine All), _) ->
        allRules
    | (Some (EnableLine (Rules rules)), _) ->
        Set.difference currentSuppressedRules rules
    | (Some (DisableLine (Rules rules)), _) ->
        Set.union currentSuppressedRules rules
    // No current line suppressions set, so use any provided next line suppression.
    | (None, Some (EnableLine All)) ->
        Set.empty
    | (None, Some (DisableLine All)) ->
        allRules
    | (None, Some (EnableLine (Rules rules))) ->
        Set.difference currentSuppressedRules rules
    | (None, Some (DisableLine (Rules rules))) ->
        Set.union currentSuppressedRules rules
    | (None, None) ->
        currentSuppressedRules

/// Gets suppression information for the current line based on the current line's suppression info comments.
let private getCurrentLineSuppressionContext (suppressionInfo:SuppressionInfo option) =
    match suppressionInfo with
    | Some (SuppressionInfo.EnableLine target) ->
        Some (LineSuppression.EnableLine target)
    | Some (SuppressionInfo.DisableLine target) ->
        Some (LineSuppression.DisableLine target)
    | _ -> None

/// Gets suppression information for the next line based on the current line's suppression info comments.
let private getNextLineSuppressionContext (allRules:Set<string>) (currentSuppressedRules:Set<string>) (suppressionInfo:SuppressionInfo option) =
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
         (currentSuppressedRules, Some (LineSuppression.EnableLine target))
    | Some (DisableNextLine target) ->
        (currentSuppressedRules, Some (LineSuppression.DisableLine target))
    | Some (SuppressionInfo.EnableLine _)
    | Some (SuppressionInfo.DisableLine _)
    | None -> (currentSuppressedRules, None)

/// Creates a dictionary from line number to a set of rules which are suppressed for that line.
let getSuppressedRulesPerLine (allRules:Set<string>) (lines:string list) =
    parseSuppressionInfo lines
    |> List.fold (fun ((currentSuppressedRules, nextLineSuppression:LineSuppression option), agg) (lineNum, suppressionInfo) ->
        let currentLineSuppression = getCurrentLineSuppressionContext suppressionInfo
        let currentLineSuppressedRules = getCurrentLineSuppressedRules allRules (currentSuppressedRules, currentLineSuppression, nextLineSuppression)
        let (nextLineSuppressedRules, nextLineSuppressionContext) = getNextLineSuppressionContext allRules currentSuppressedRules suppressionInfo
        ((nextLineSuppressedRules, nextLineSuppressionContext), (lineNum, currentLineSuppressedRules) :: agg)) ((Set.empty, None), [])
    |> snd
    |> dict
