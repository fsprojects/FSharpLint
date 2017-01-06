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
          SuggestedFix: SuggestedFix option }
    
    /// Passed to each analyser to provide them with access to the configuration and a way of reporting errors.
    [<NoEquality; NoComparison>]
    type AnalyserInfo =
        {  /// Version of F# the source that's being analysed was written in.
          FSharpVersion: Version

          /// The current lint config to be used by visitors.
          Config: Configuration.Configuration

          /// Used by visitors to report warnings.
          Suggest: LintSuggestion -> unit
          
          /// Source of the current file being analysed.
          Text: string }

        member this.UseTypeChecker = 
            match this.Config.UseTypeChecker with
            | Some(true) -> true
            | Some(_) | None -> false

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