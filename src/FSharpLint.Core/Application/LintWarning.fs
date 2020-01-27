namespace FSharpLint.Application

/// Contains functionality to help report lint warnings.
module LintWarning =

    open System
    open FSharp.Compiler.Range
    open FSharpLint.Framework

    /// Gets a message stating where a lint warning occured.
    let getWarningMessage (range:range) =
        let error = FSharpLint.Framework.Resources.GetString("LintError")
        String.Format(error, range.FileName, range.StartLine, range.StartColumn)

    /// Generates a message including highlighting where in the code the warning was found.
    let warningInfoLine getErrorMessage (range:range) (input:string) =
        let errorenousLine = input.Split('\n').[range.StartLine - 1].TrimEnd('\r')
        let highlightColumnLine =
            if String.length errorenousLine = 0 then "^"
            else
                errorenousLine
                |> Seq.mapi (fun i x -> if i = range.StartColumn then "^" else " ")
                |> Seq.reduce (+)

        (getErrorMessage range) + Environment.NewLine + errorenousLine + Environment.NewLine + highlightColumnLine

    /// Generates a message including highlighting where in the code the warning was found along with
    /// stating the location of where the warning occurred. `warningInfoLine` and `getWarningMessage` combined.
    let getWarningWithLocation = warningInfoLine getWarningMessage

    /// A lint warning - information on where a lint rule was found to be broken.
    [<NoEquality; NoComparison>]
    type Warning =
        { /// Warning to display to the user.
          info: string

          /// Location of the warning.
          range: range

          /// Entire input file, needed to display where in the file the error occurred.
          input: string

          /// Suggested fix for the warning.
          fix: Suggestion.SuggestedFix option}