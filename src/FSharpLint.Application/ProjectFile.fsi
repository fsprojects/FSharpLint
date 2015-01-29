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
        | MSBuildFailedToLoadReferencedProjectFile of string * Microsoft.Build.Exceptions.InvalidProjectFileException
        | UnableToFindProjectOutputPath of string
        | UnableToFindReferencedProject of string
        | UnableToFindFSharpCoreDirectory
        | FailedToLoadConfig of string
        | RunTimeConfigError
        | FailedToResolveReferences

    type internal Result<'TSuccess> = 
        | Success of 'TSuccess
        | Failure of Error

    type FSharpFile =
        {
            FileLocation: string
            ExcludeFromAnalysis: bool
        }

    val internal loadRulesAssembly : unit -> System.Reflection.Assembly

    type internal ProjectFile = 
        {
            Path: string
            References: string list
            ProjectReferences: string list
            FSharpFiles: FSharpFile list
            Config: Map<string, FSharpLint.Framework.Configuration.Analyser>
        }

    val internal loadProjectFile : string -> userSuppliedFSharpCoreDirectory: string option -> Result<ProjectFile>