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

namespace FSharpLint.MSBuild

open System
open System.IO
open System.Reflection
open System.Security.Policy
open Microsoft.Build.Framework
open Microsoft.Build.Utilities

type FSharpLintTask() = 
    inherit Task()

    [<Required>]
    member val Project = "" with get, set

    member val TreatWarningsAsErrors = false with get, set

    override this.Execute() = 
        let assembly = typeof<FSharpLintTask>.Assembly
        let fullPath = assembly.Location
        let directory = Path.GetDirectoryName fullPath

        let adSetup = AppDomainSetup(ApplicationBase = directory,
                                     ConfigurationFile = Path.Combine(directory, "app.config"))

        let ad = AppDomain.CreateDomain("FSharpLint.MSBuild", Evidence(), adSetup)
        let remoteLintRunner = 
            ad.CreateInstanceAndUnwrap(assembly.FullName, "FSharpLint.MSBuild.AppDomain+LintRunner")
            :?> AppDomain.LintRunner

        for warning in remoteLintRunner.Lint(this.Project) do
            if this.TreatWarningsAsErrors then
                this.Log.LogError("", "", "", 
                                    warning.Filename, 
                                    warning.StartLine, warning.StartColumn, 
                                    warning.EndLine, warning.EndColumn, warning.Info, null)
            else
                this.Log.LogWarning("", "", "", 
                                    warning.Filename, 
                                    warning.StartLine, warning.StartColumn, 
                                    warning.EndLine, warning.EndColumn, warning.Info, null)

        true