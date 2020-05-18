module FSharpLint.Framework.Suppression

open System
open System.Text.RegularExpressions

/// Represents rule suppression information.
type SuppressionInfo =
    /// Re-enables rules for the rest of the file.
    | Enable of Set<String>
    /// Disables rules for the rest of the file.
    | Disable of Set<String>
    /// Disables rules for a single line.
    | DisableLine of Set<String>

/// Specifies the suppressions for an individual line.
type LineSuppression = { Line: int; Suppressions: SuppressionInfo [] }

/// Extracts rule names from a whitespace separated string of rule names.
let private extractRules (rules:Set<String>) (str:string) =
    let (splitOnWhitespace:char[]) = null
    let entries =
        str.Split(splitOnWhitespace, StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun entry -> entry.ToLowerInvariant())
        |> Seq.filter (fun entry -> rules.Contains(entry))
        |> Set.ofSeq

    // If no rules set, then all rules are applied.
    if Seq.isEmpty entries then rules else entries

/// Parses a given file to find lines containing rule suppressions.
let parseSuppressionInfo (rules:Set<String>) (lines:string []) =
    let rules = rules |> Set.map (fun rule -> rule.ToLowerInvariant())

    lines
    |> Array.mapi (fun lineNum line -> (lineNum + 1, line))
    |> Array.filter (fun (_, line) -> line.Contains("fsharplint:"))
    |> Array.choose (fun (lineNum, line) ->
        let matched = Regex.Match (line, ".*fsharplint:([a-z\-]+)\s*(.*)$")
        if matched.Success then
            let suppressionTarget =
                if matched.Groups.Count = 3 then
                    extractRules rules matched.Groups.[2].Value
                else
                    rules
            match matched.Groups.[1].Value with
            | "enable" -> Some (lineNum, Enable suppressionTarget)
            | "disable" -> Some (lineNum, Disable suppressionTarget)
            | "disable-line" -> Some (lineNum, DisableLine suppressionTarget)
            | "disable-next-line" -> Some (lineNum + 1, DisableLine suppressionTarget)
            | _ -> None
        else None)
    |> Array.groupBy (fun (line, _) -> line)
    |> Array.map (fun (line, suppressions) ->
        { Line = line
          Suppressions = suppressions |> Array.map snd })

/// Check if a rule is suppressed for a given line.
/// Given line suppressions must be in order by line - see parseSuppressionInfo.
let isSuppressed (rule:String) (line:int) (lineSuppressions:LineSuppression []) =
    if Array.isEmpty lineSuppressions then
        false
    else
        let rule = rule.ToLowerInvariant()

        let disabledRules =
            lineSuppressions
            |> Array.takeWhile (fun lineSupression -> lineSupression.Line <= line)
            |> Array.fold (fun (disabledRules:Set<String>) (lineSuppression:LineSuppression) ->
                lineSuppression.Suppressions |> Array.fold (fun (disabledRules:Set<String>) suppression ->
                    match suppression with
                    | Enable rules ->
                        Set.difference disabledRules rules
                    | Disable rules ->
                        Set.union disabledRules rules
                    | DisableLine rules ->
                        if line = lineSuppression.Line then
                            Set.union disabledRules rules
                        else
                            disabledRules
                ) disabledRules) Set.empty

        disabledRules.Contains(rule)
