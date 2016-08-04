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

type private Proxy(project, onFailure) =
    inherit MarshalByRefObject()

    member this.Lint() =
        let assembly = typeof<Proxy>.Assembly
        let fullPath = assembly.Location
        let directory = Path.GetDirectoryName fullPath

        let adSetup = AppDomainSetup(ApplicationBase = directory,
                                     ConfigurationFile = Path.Combine(directory, "app.config"))

        let ad = AppDomain.CreateDomain("FSharpLint.MSBuild", Evidence(), adSetup)
        let remoteLintRunner = 
            ad.CreateInstanceAndUnwrap(assembly.FullName, "FSharpLint.MSBuild.AppDomain+LintRunner")
            :?> AppDomain.LintRunner

        remoteLintRunner.Failure.Add onFailure

        remoteLintRunner.Lint(project)    

[<Serializable>]
type FSharpLintTask() = 
    inherit Task()

    [<Required>]
    member val Project = "" with get, set

    member val TreatWarningsAsErrors = false with get, set

    override this.Execute() = 
        try
            let warnings = Proxy(this.Project, this.OnFailure).Lint()

            for warning in warnings do
                if this.TreatWarningsAsErrors then
                    this.Log.LogError("", "", "", 
                                      warning.Filename, 
                                      warning.StartLine, warning.StartColumn, 
                                      warning.EndLine, warning.EndColumn, warning.Info)
                else
                    this.Log.LogWarning("", "", "", 
                                        warning.Filename, 
                                        warning.StartLine, warning.StartColumn, 
                                        warning.EndLine, warning.EndColumn, warning.Info)

            true
        with e -> 
            this.Log.LogErrorFromException(e, showStackTrace = true, showDetail = true, file = this.Project)
            false

    member this.OnFailure(args: AppDomain.FailureEventArgs) =
        this.Log.LogWarning(sprintf "FSharpLint.MSBuild failed to lint file %s." args.Filename)
        this.Log.LogWarningFromException(args.Exception, showStackTrace = true)