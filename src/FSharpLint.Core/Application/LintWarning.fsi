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

    open Microsoft.FSharp.Compiler.Range
    open FSharpLint.Framework


    /// Gets a message stating where a lint warning occured.
    val getWarningMessage : range -> string

    /// Generates a message including highlighting where in the code the warning was found.
    val warningInfoLine : getErrorMessage:(range -> string) -> range -> input:string -> string

    /// Generates a message including highlighting where in the code the warning was found along with
    /// stating the location of where the warning occurred. (warningInfoLine and getWarningMessage) combined.
    val getWarningWithLocation : (range -> string -> string)

    /// A lint warning - information on where a lint rule was found to be broken.
    [<NoComparison>]
    type Warning =
        { /// Warning to display to the user.
          Info: string

          /// Location of the warning.
          Range: range

          /// Entire input file, needed to display where in the file the error occurred.
          Input: string

          /// Suggested fix for the warning.
          Fix: Ast.SuggestedFix option}