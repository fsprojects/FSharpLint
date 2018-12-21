namespace FSharpLint.Framework

module Analyser =

    open System
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices

    /// Information for consuming applications to provide an automated fix for a lint suggestion.
    [<NoEquality; NoComparison>]
    type SuggestedFix =
        { /// Text to be replaced.
          FromText: string 

          /// Location of the text to be replaced.
          FromRange: range

          /// Text to replace the `FromText`, i.e. the fix.
          ToText: string }

    /// A lint "warning", sources the location of the warning with a suggestion on how it may be fixed.
    [<NoEquality; NoComparison>]
    type LintSuggestion = 
        { /// Location of the code that prompted the suggestion.
          Range: range

          /// Suggestion message to describe the possible problem to the user.
          Message: string

          /// Information to provide an automated fix.
          SuggestedFix: Lazy<SuggestedFix option> option
          
          /// Async type checks to be performed to confirm this suggestion is valid.
          /// Suggestion is only considered valid when all type checks resolve to true.
          TypeChecks: Async<bool> list }

        member internal this.WithTypeCheck typeCheck =
            match typeCheck with
            | Some(check) -> { this with TypeChecks = check::this.TypeChecks }
            | None -> this
    
    /// Passed to each analyser to provide them with access to the configuration and a way of reporting errors.
    [<NoEquality; NoComparison>]
    type AnalyserInfo =
        { /// The current lint config to be used by visitors.
          Config: Configuration.Configuration

          /// Used by visitors to report warnings.
          Suggest: LintSuggestion -> unit
          
          /// Source of the current file being analysed.
          Text: string }

        /// Tries to find the source code within a given range.
        member this.TryFindTextOfRange(range:range) =
            let startIndex = ExpressionUtilities.findPos range.Start this.Text
            let endIndex = ExpressionUtilities.findPos range.End this.Text

            match startIndex, endIndex with
            | Some(startIndex), Some(endIndex) -> 
                this.Text.Substring(startIndex, endIndex - startIndex) |> Some
            | _ -> None
          
    [<NoEquality; NoComparison>]
    type AnalyserArgs =
        { Info: AnalyserInfo
          CheckFile: FSharpCheckFileResults option
          SyntaxArray: AbstractSyntaxArray.Node []
          SkipArray: AbstractSyntaxArray.Skip [] }