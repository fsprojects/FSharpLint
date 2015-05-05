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

module AppDomainWorker =

    open System
    open FSharpLint.Worker

    type LintOptions =
        {
            /// Callback that's called at the start and end of parsing each file (or when a file fails to be parsed).
            Progress: System.Action<Progress>

            /// Callback that's called when a lint error is detected.
            ErrorReceived: System.Action<Error>

            /// Fail the build if one or more lint warnings are found in a project.
            FailBuildIfAnyWarnings: bool
        }

    type AppDomainWorker = 
        inherit MarshalByRefObject
        interface IDisposable

        new: unit -> AppDomainWorker

        [<DefaultValue>] val mutable Options : LintOptions

        member RunLint: projectFile:string -> options:LintOptions -> Result

        [<Runtime.Remoting.Messaging.OneWay>]
        member ReportError: error:Error -> unit

        [<Runtime.Remoting.Messaging.OneWay>]
        member ReportProgress: progress:Progress -> unit