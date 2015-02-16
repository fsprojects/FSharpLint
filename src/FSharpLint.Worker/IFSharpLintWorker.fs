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

namespace FSharpLint.Worker

type Range =
    {
        StartLine: int
        StartColumn: int
        EndLine: int
        EndColumn: int
        FileName: string
    }

type Error =
    {
        /// Description of the error.
        Info: string

        Range: Range

        /// Entire input file, needed to display where in the file the error occurred.
        Input: string

        FormattedError: string
    }

type Progress =
    | Starting of string
    | ReachedEnd of string
    | Failed of string * System.Exception

type LintOptions =
    {
        /// Function that when returns true cancels the parsing of the project, useful for cancellation tokens etc.
        FinishEarly: System.Func<bool>

        /// Callback that's called at the start and end of parsing each file (or when a file fails to be parsed).
        Progress: System.Action<Progress>

        /// Callback that's called when a lint error is detected.
        ErrorReceived: System.Action<Error>
    }

type Result =
    | Success
    | Failure of string

type IFSharpLintWorker =
    abstract member RunLint : projectFile:string -> options:LintOptions -> Result