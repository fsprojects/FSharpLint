// FSharpLint, a linter for F#.
// Copyright (C) 2016 Matthew Mcveigh
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace FSharpLint.Application

/// Contains functionality to help report lint warnings.
module LintWarning =

    open System
    open Microsoft.FSharp.Compiler.Range
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
          Info: string

          /// Location of the warning.
          Range: range

          /// Entire input file, needed to display where in the file the error occurred.
          Input: string

          /// Suggested fix for the warning.
          Fix: Ast.SuggestedFix option}