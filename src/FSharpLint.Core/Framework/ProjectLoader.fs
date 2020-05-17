module FSharpLint.Framework.ProjectLoader

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open Dotnet.ProjInfo.Inspect.MSBuild

let private runProcess (workingDir:string) (exePath:string) (args:string) =
    let psi = System.Diagnostics.ProcessStartInfo()
    psi.FileName <- exePath
    psi.WorkingDirectory <- workingDir
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.Arguments <- args
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false

    use p = new System.Diagnostics.Process()
    p.StartInfo <- psi
    let sbOut = System.Text.StringBuilder()
    p.OutputDataReceived.Add(fun ea -> sbOut.AppendLine(ea.Data) |> ignore)
    let sbErr = System.Text.StringBuilder()
    p.ErrorDataReceived.Add(fun ea -> sbErr.AppendLine(ea.Data) |> ignore)
    p.Start() |> ignore
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()

    let exitCode = p.ExitCode

    (exitCode, (workingDir, exePath, args))

let getProjectFileInfo (releaseConfig:string option) (projectFilePath:string) =
    let projDir = System.IO.Path.GetDirectoryName projectFilePath

    let msBuildParams =
        releaseConfig
        |> Option.map (fun config -> [MSbuildCli.Property("ConfigurationName", config)])
        |> Option.defaultValue []

    let msBuildResults =
        let runCmd exePath args = runProcess projDir exePath (args |> String.concat " ")
        let msbuildExec = Dotnet.ProjInfo.Inspect.dotnetMsbuild runCmd

        projectFilePath
        |> Dotnet.ProjInfo.Inspect.getProjectInfos ignore msbuildExec [Dotnet.ProjInfo.Inspect.getFscArgs] msBuildParams

    match msBuildResults with
    | Result.Ok [getFscArgsResult] ->
        match getFscArgsResult with
        | Result.Ok (Dotnet.ProjInfo.Inspect.GetResult.FscArgs fa) ->

            let projDir = Path.GetDirectoryName projectFilePath

            let isSourceFile (option:string) =
                option.TrimStart().StartsWith("-") |> not

            let compileFilesToAbsolutePath (f:string) =
                if Path.IsPathRooted f then
                    f
                else
                    Path.Combine(projDir, f)

            { ProjectFileName = projectFilePath
              SourceFiles = fa |> List.filter isSourceFile |> List.map compileFilesToAbsolutePath |> Array.ofList
              OtherOptions = fa |> List.filter (isSourceFile >> not) |> Array.ofList
              ReferencedProjects = [||]
              IsIncompleteTypeCheckEnvironment = false
              UseScriptResolutionRules = false
              LoadTime = DateTime.Now
              UnresolvedReferences = None
              OriginalLoadReferences = []
              ExtraProjectInfo = None
              ProjectId = None
              Stamp = None }
        | Result.Ok _ ->
            failwithf "error getting FSC args from msbuild info"
        | Result.Error error ->
            failwithf "error getting FSC args from msbuild info, %A" error
    | Result.Ok r ->
        failwithf "error getting msbuild info: more info returned than expected %A" r
    | Result.Error r ->
        failwithf "error getting msbuild info: %A" r

