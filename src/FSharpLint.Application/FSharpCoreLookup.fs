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

/// Unfortunately it seems we need to have a reference to FSharp.Core that has .sigdata and .optdata
/// in the same directory to parse the code using FSharp.Compiler.Services. As a result we have this 
/// to attempt to correctly lookup the correct directory for such a reference.
module FSharpCoreLookup =

    open System.Text.RegularExpressions

    /// Gets the full path to the directory from the paths in the versions map.
    let private fullDirectory directory = 
        Microsoft.FSharp.Compiler.MSBuildResolver.DotNetFrameworkReferenceAssembliesRootDirectory
            + @"\..\..\FSharp\" + directory

    /// Get the first directory that exists.
    let private getExistingDirectory directories =
        directories
            |> List.map fullDirectory
            |> List.tryPick (fun x -> if System.IO.Directory.Exists(x) then Some(x) else None)

    /// Standard locations of FSharp.Core for different versions.
    let private versions =
        [ 
            "2.3.0.0", [@".NETFramework\v2.0\2.3.0.0";@"3.0\Runtime\v2.0";@"2.0\Runtime\v2.0"]
            "2.3.5.1", [@".NETPortable\2.3.5.1"]
            "2.3.5.0", [@".NETPortable\2.3.5.0";@"3.0\Runtime\.NETPortable"]
            "3.3.1.0", [@".NETCore\3.3.1.0"]
            "4.3.0.0", [@".NETFramework\v4.0\4.3.0.0";@"3.0\Runtime\v4.0";@"2.0\Runtime\v4.0"]
            "4.3.1.0", [@".NETFramework\v4.0\4.3.1.0"]
        ] |> Map.ofList

    /// Tries to find the latest FSharp.Core avaliable on the machine.
    let tryFindLatestVersion () = 
        [
            "4.3.1.0"
            "4.3.0.0"
            "2.3.0.0"
        ] |> List.tryPick (fun x -> getExistingDirectory versions.[x])

    /// Gets the version number from the FSharpCore reference string.
    let versionFromReference referenceString = 
        let versionMatch = Regex.Match(referenceString, @"version\s*=\s*(\d\.\d\.\d\.\d)", RegexOptions.IgnoreCase)

        let versionFoundInString = versionMatch.Success && versionMatch.Groups.Count = 2

        if versionFoundInString then
            Some(versionMatch.Groups.[1].Value)
        else
            None

    let private lookupDirectory referenceString =
        match versionFromReference referenceString with
            | Some(version) when versions |> Map.containsKey version ->
                let directories = versions.[version]
                getExistingDirectory directories
            | Some(_) -> None /// Unrecognised version.
            | None -> tryFindLatestVersion()

    /// Get the FSharpCore reference from a list of references.
    let private tryGetFSharpCoreReference references =
        let isFSharpCoreReference (ref:string, _) = 
            if ref.StartsWith("FSharp.Core") then Some(ref) else None

        references |> Array.tryPick isFSharpCoreReference

    /// Attempts to get the directory containing FSharpCore with .sigdata and .opdata for the correct version.
    /// References should be a list of references containing the reference to FSharpCore that's going to need to be resolved.
    let lookup references =
        let isNotRunningOnMono = System.Type.GetType("Mono.Runtime") = null

        if isNotRunningOnMono then
            match references |> tryGetFSharpCoreReference with 
                | Some(fsharpCoreReference) -> lookupDirectory fsharpCoreReference
                | None -> None
        else
            /// Seems to magically work on mono even when the directory doesn't exist
            /// FSharp.core lookup for mono needs further investigation
            Some(fullDirectory @"3.1\Runtime\v4.0")