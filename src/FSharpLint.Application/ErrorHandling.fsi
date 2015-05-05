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

/// Contains the functionality for reporting lint errors.
module ErrorHandling =

    open System
    open Microsoft.FSharp.Compiler.Range

    val getErrorMessage: range:range -> string

    /// Generates error reporting information on where in a file an error has occured.
    val errorInfoLine: getErrorMessage:(range -> string) -> range:range -> input:string -> string

    val getCompleteErrorText: (range -> string -> string)

    type Error =
        {
            /// Description of the error.
            Info: string

            /// Where the error was found.
            Range: range

            /// Entire input file, needed to display where in the file the error occurred.
            Input: string
        }