namespace FSharpLint.Application

/// Contains functionality to help report lint warnings.
module LintWarning =

    open FSharp.Compiler.Range
    open FSharpLint.Framework


    /// Gets a message stating where a lint warning occured.
    val getWarningMessage : range -> string

    /// Generates a message including highlighting where in the code the warning was found.
    val warningInfoLine : getErrorMessage:(range -> string) -> range -> input:string -> string

    /// Generates a message including highlighting where in the code the warning was found along with
    /// stating the location of where the warning occurred. (warningInfoLine and getWarningMessage) combined.
    val getWarningWithLocation : (range -> string -> string)

    /// A lint warning - information on where a lint rule was found to be broken.
    [<NoEquality; NoComparison>]
    type Warning =
        { /// Warning to display to the user.
          Info: string

          /// Location of the warning.
          Range: range

          /// Entire input file, needed to display where in the file the error occurred.
          Input: string

          /// Suggested fix for the warning.
          Fix: Analyser.SuggestedFix option}