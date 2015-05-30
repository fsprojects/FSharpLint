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

namespace FSharpLint.FunctionalTest

module TestPackageHelper =

    open System.IO
    open NUnit.Framework

    let getPath (path:string) = path.Replace('/', Path.DirectorySeparatorChar)

    let copy toDirectory path = File.Copy(path, Path.Combine(toDirectory, Path.GetFileName(path)), true)

    let copyFSharpLintTaskFiles toDirectory = 
        let taskDirectory = 
            sprintf @"../../../FSharpLint.FunctionalTest.TestedProject/%s/" toDirectory
                |> getPath

        let taskDirectoryExists = Directory.Exists(taskDirectory)
        if not taskDirectoryExists then
            Directory.CreateDirectory(taskDirectory) |> ignore

        let copyToTaskDir = getPath >> (copy taskDirectory)

        let binDir = 
            #if DEBUG
                "Debug"
            #else
                "Release"
            #endif

        copyToTaskDir (@"../../../../src/FSharpLint.MSBuildIntegration/bin/" + binDir + "/FSharp.Core.dll")
        copyToTaskDir (@"../../../../src/FSharpLint.MSBuildIntegration/bin/" + binDir + "/FSharpLint.MSBuildIntegration.dll")
        copyToTaskDir (@"../../../../src/FSharpLint.MSBuildIntegration/bin/" + binDir + "/FSharpLint.Worker.dll")

        copyToTaskDir (@"../../../../src/FSharpLint.FAKE/bin/" + binDir + "/FSharpLint.FAKE.dll")
        copyToTaskDir (@"../../../../src/FSharpLint.CrossDomain/bin/" + binDir + "/FSharpLint.CrossDomain.dll")
        copyToTaskDir (@"../../../../src/FSharpLint.Application/bin/" + binDir + "/FSharpLint.Application.dll")
        copyToTaskDir (@"../../../../src/FSharpLint.Rules/bin/" + binDir + "/FSharpLint.Rules.dll")
        copyToTaskDir (@"../../../../src/FSharpLint.Rules/bin/" + binDir + "/FSharpLint.Framework.dll")
        copyToTaskDir (@"../../../../src/FSharpLint.Rules/bin/" + binDir + "/FSharp.Compiler.Service.dll")
        copyToTaskDir (@"../../../../src/FSharpLint.Rules/bin/" + binDir + "/FParsecCS.dll")
        copyToTaskDir (@"../../../../src/FSharpLint.Rules/bin/" + binDir + "/FParsec.dll")

        let taskBuildDirectory = Path.Combine(taskDirectory, "build")

        let taskBuildDirectoryExists = Directory.Exists(taskBuildDirectory)
        if not taskBuildDirectoryExists then
            Directory.CreateDirectory(taskBuildDirectory) |> ignore

        let copyToTaskBuildDir = getPath >> (copy taskBuildDirectory)

        copyToTaskBuildDir @"../../../../packaging/tool/build/FSharpLint.targets"