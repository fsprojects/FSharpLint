module FSharpLint.Framework.Suggestion

open System
open FSharp.Compiler.Text

/// Information for consuming applications to provide an automated fix for a lint suggestion.
[<NoEquality; NoComparison>]
type SuggestedFix = {
    /// Location of the text to be replaced.
    FromRange:Range

    /// Text to replace the `FromText`, i.e. the fix.
    ToText:string
}

[<NoEquality; NoComparison>]
type WarningDetails = {
    /// Location of the code that prompted the suggestion.
    Range:Range

    /// Suggestion message to describe the possible problem to the user.
    Message:string

    /// Information to provide an automated fix.
    SuggestedFix:Lazy<SuggestedFix option> option

    /// Type checks to be performed to confirm this suggestion is valid.
    /// Suggestion is only considered valid when all type checks resolve to true.
    TypeChecks:(unit -> bool) list
} with
    member internal this.WithTypeCheck typeCheck =
        match typeCheck with
        | Some(check) -> { this with TypeChecks = check::this.TypeChecks }
        | None -> this

/// A lint "warning", sources the location of the warning with a suggestion on how it may be fixed.
[<NoEquality; NoComparison>]
type LintWarning = {
    /// Unique identifier for the rule that caused the warning.
    RuleIdentifier:string

    /// Unique name for the rule that caused the warning.
    RuleName:string

    /// Path to the file where the error occurs.
    FilePath:string

    /// Text that caused the error (the `Range` of the content of `FileName`).
    ErrorText:string

    /// Details for the warning.
    Details:WarningDetails
}
