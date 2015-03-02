(*
    FSharpLint, a linter for F#.
    Copyright (C) 2014 Matthew Mcveigh

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

namespace FSharpLint.Application

/// Runs the lint on an entire project using a .fsproj file.
module RunLint =

    /// Provides information on what the linter is currently doing.
    type ParserProgress =
        /// Started parsing a file.
        | Starting of string

        /// Finished parsing a file.
        | ReachedEnd of string

        /// Failed to parse a file.
        | Failed of string * System.Exception

        member Filename : unit -> string

    type Result = 
        | Success
        | Failure of ProjectFile.Error

    /// Provides information for controlling the parse of a project.
    type ProjectParseInfo =
        {
            /// Function that when returns true cancels the parsing of the project, useful for cancellation tokens etc.
            FinishEarly: System.Func<bool>

            /// Absolute path to the .fsproj file.
            ProjectFile: string

            /// Callback that's called at the start and end of parsing each file (or when a file fails to be parsed).
            Progress: System.Action<ParserProgress>

            /// Callback that's called when a lint error is detected.
            ErrorReceived: System.Action<ErrorHandling.Error>
        }
        
    /// Parses and runs the linter on all the files in a project.
    val parseProject : projectInformation: ProjectParseInfo -> Result
        
    /// Parses and runs the linter on a single file.
    val parseFile : pathToFile: string -> errorReceived: System.Action<ErrorHandling.Error> -> unit
        
    /// Parses and runs the linter on a string.
    val parseInput : input: string -> errorReceived: System.Action<ErrorHandling.Error> -> unit