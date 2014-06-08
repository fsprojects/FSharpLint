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

namespace FSharpLint.MSBuildIntegration

type FSharpLintTask() = 
    inherit Microsoft.Build.Utilities.Task()

    let currentDomain = System.AppDomain.CurrentDomain

    let handler = System.ResolveEventHandler(fun _ args ->
        if System.Reflection.AssemblyName(args.Name).Name = "FSharp.Core" then
            typeof<Microsoft.FSharp.Core.FSharpFunc<_,_>>.Assembly
        else 
            null)

    [<Microsoft.Build.Framework.Required>]
    member val Project = "" with get, set

    member val TreatWarningsAsErrors = false with get, set

    override this.Execute() = 
        let finishEarly = System.Func<_>(fun _ -> false)
        let action = System.Action<_>(fun _ -> ())
        let error = System.Action<FSharpLint.Application.ErrorHandling.Error>(fun error -> 
            let (log:string*string*string*string*int*int*int*int*string*obj[]->unit) =
                if this.TreatWarningsAsErrors then
                    this.Log.LogError
                else
                    this.Log.LogWarning

            let range = error.Range
            log("", "", "", 
                range.FileName, 
                range.StartLine, 
                range.StartColumn + 1,
                range.EndLine,
                range.EndColumn + 1, 
                error.Info,
                null))

        currentDomain.add_AssemblyResolve(handler)

        try
            FSharpLint.Application.RunLint.parseProject(finishEarly, this.Project, action, error)
                |> ignore
        with
            | e -> 
                this.Log.LogWarning("Lint failed while analysing " + this.Project + ".\nFailed with: " + e.Message + "\nStack trace: " + e.StackTrace)
                
        currentDomain.remove_AssemblyResolve(handler)

        true