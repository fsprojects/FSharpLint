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

namespace FSharpLint.FunctionalTest

module TestPackageHelper =

    open System.IO
    open NUnit.Framework

    let (</>) x y = Path.Combine(x, y)

    let basePath = TestContext.CurrentContext.TestDirectory </> ".." </> ".." </> ".." </> ".."

    let copy toDirectory path = File.Copy(path, toDirectory </> Path.GetFileName path, true)

    let copyFSharpLintTaskFiles toDirectory = 
        let taskDirectory = basePath </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> toDirectory

        if not (Directory.Exists taskDirectory) then
            Directory.CreateDirectory taskDirectory |> ignore

        let copyToTaskDir = copy taskDirectory

        let binDir = 
            #if DEBUG
                "Debug"
            #else
                "Release"
            #endif

        let getAssemblyPath projectName assemblyName =
            basePath </> "src" </> projectName </> "bin" </> binDir </> assemblyName
            
        getAssemblyPath "FSharpLint.MSBuild" "FSharpLint.MSBuild.dll" |> copyToTaskDir
        getAssemblyPath "FSharpLint.MSBuild" "FSharp.Core.dll" |> copyToTaskDir
        getAssemblyPath "FSharpLint.MSBuild" "app.config" |> copyToTaskDir
        
        getAssemblyPath "FSharpLint.Fake" "FSharpLint.Fake.dll" |> copyToTaskDir
        
        getAssemblyPath "FSharpLint.Core" "FSharpLint.Core.dll" |> copyToTaskDir
        getAssemblyPath "FSharpLint.Core" "FSharp.Compiler.Service.ProjectCracker.dll" |> copyToTaskDir
        getAssemblyPath "FSharpLint.Core" "FSharp.Compiler.Service.ProjectCrackerTool.exe" |> copyToTaskDir
        getAssemblyPath "FSharpLint.Core" "FSharp.Compiler.Service.ProjectCrackerTool.exe.config" |> copyToTaskDir
        getAssemblyPath "FSharpLint.Core" "FSharp.Compiler.Service.dll" |> copyToTaskDir
        getAssemblyPath "FSharpLint.Core" "FParsecCS.dll" |> copyToTaskDir
        getAssemblyPath "FSharpLint.Core" "FParsec.dll" |> copyToTaskDir

        let taskBuildDirectory = taskDirectory </> "build"

        if not (Directory.Exists taskBuildDirectory) then
            Directory.CreateDirectory taskBuildDirectory |> ignore

        basePath </> "packaging" </> "tool" </> "build" </> "FSharpLint.targets" |> copy taskBuildDirectory