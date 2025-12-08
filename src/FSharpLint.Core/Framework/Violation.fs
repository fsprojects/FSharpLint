module FSharpLint.Framework.Violation

open System
open FSharp.Compiler.Text

/// Information for consuming applications to provide an automated fix for a lint suggestion.
[<NoEquality; NoComparison>]
type SuggestedFix = {
    /// Text to be replaced.
    FromText:string

    /// Location of the text to be replaced.
    FromRange:Range

    /// Text to replace the `FromText`, i.e. the fix.
    ToText:string
}

[<NoEquality; NoComparison>]
type ViolationDetails = {
    /// Location of the code that prompted the violation.
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

/// A lint "violation", sources the location of the vilation with a suggestion on how it may be fixed.
[<NoEquality; NoComparison>]
type LintViolation = {
    /// Unique identifier for the rule that found the violation.
    RuleIdentifier:string

    /// Unique name for the rule that found the violation.
    RuleName:string

    /// Path to the file where the violation occurs.
    FilePath:string

    /// Source code fragment that caused the violation (the `Range` of the content of `FileName`).
    SourceCodeFragment:string

    /// Details for the rule violation.
    Details:ViolationDetails
}
