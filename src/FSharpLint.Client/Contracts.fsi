module FSharpLint.Client.Contracts

open System.Threading
open System.Threading.Tasks

module Methods =

    [<Literal>]
    val Version: string = "fsharplint/version"
    
    [<Literal>]
    val LintFile: string = "fsharplint/lintfile"

type VersionRequest =
    {
        FilePath: string
    }

type LintFileRequest =
    {
        FilePath: string
        LintConfigPath: string option
    }


type ClientRange =
    class
        new: startLine: int * startColumn: int * endLine: int * endColumn: int -> ClientRange
        val StartLine: int
        val StartColumn: int
        val EndLine: int
        val EndColumn: int
    end

type ClientSuggestedFix = {
    /// Text to be replaced.
    FromText:string

    /// Location of the text to be replaced.
    FromRange:ClientRange

    /// Text to replace the `FromText`, i.e. the fix.
    ToText:string
}

[<NoEquality; NoComparison>]
type ClientWarningDetails = {
    /// Location of the code that prompted the suggestion.
    Range:ClientRange

    /// Suggestion message to describe the possible problem to the user.
    Message:string

    /// Information to provide an automated fix.
    SuggestedFix:ClientSuggestedFix option
}

/// A lint "warning", sources the location of the warning with a suggestion on how it may be fixed.
[<NoEquality; NoComparison>]
type ClientLintWarning = {
    /// Unique identifier for the rule that caused the warning.
    RuleIdentifier:string

    /// Unique name for the rule that caused the warning.
    RuleName:string

    /// Path to the file where the error occurs.
    FilePath:string

    /// Text that caused the error (the `Range` of the content of `FileName`).
    ErrorText:string

    /// Details for the warning.
    Details:ClientWarningDetails
}

type FSharpLintResult =
    | Content of string
    | LintResult of ClientLintWarning list 

type FSharpLintResponse = { 
    Code: int
    FilePath: string
    Result : FSharpLintResult
}

type FSharpLintService =
    inherit System.IDisposable

    abstract VersionAsync: VersionRequest * ?cancellationToken: CancellationToken -> Task<FSharpLintResponse>
    abstract LintFileAsync: LintFileRequest * ?cancellationToken: CancellationToken -> Task<FSharpLintResponse>
