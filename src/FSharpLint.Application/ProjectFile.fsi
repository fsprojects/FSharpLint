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

module ProjectFile =

    type Error =
        | ProjectFileCouldNotBeFound of string
        | MSBuildFailedToLoadProjectFile of string * Microsoft.Build.Exceptions.InvalidProjectFileException
        | UnableToFindProjectOutputPath of string
        | UnableToFindReferencedProject of string
        | FailedToLoadConfig of string
        | RunTimeConfigError
        | FailedToResolveReferences

    type internal Result<'TSuccess> = 
        | Success of 'TSuccess
        | Failure of Error

    val internal loadRulesAssembly : unit -> System.Reflection.Assembly

    val internal loadConfigForProject : projectFilePath:string -> Result<FSharpLint.Framework.Configuration.Configuration>