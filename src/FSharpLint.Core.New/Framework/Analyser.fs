module FSharpLint.Framework.Suggestion

open System
open FSharp.Compiler.Range

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
