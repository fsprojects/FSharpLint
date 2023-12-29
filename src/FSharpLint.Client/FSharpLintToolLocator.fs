module FSharpLint.Client.FSharpLintToolLocator

open System
open System.ComponentModel
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open System.Runtime.InteropServices
open StreamJsonRpc
open FSharpLint.Client.LSPFSharpLintServiceTypes

let private supportedRange = SemanticVersioning.Range(">=v0.21.3") //TODO: proper version

let private (|CompatibleVersion|_|) (version: string) =
    match SemanticVersioning.Version.TryParse version with
    | true, parsedVersion ->
        if supportedRange.IsSatisfied(parsedVersion, includePrerelease = true) then
            Some version
        else
            None
    | _ -> None
let [<Literal>] fsharpLintToolName = "dotnet-fsharplint"
    
let private (|CompatibleToolName|_|) toolName =
    if toolName = fsharpLintToolName then
        Some toolName
    else
        None

let private readOutputStreamAsLines (outputStream: StreamReader) : string list =
    let rec readLines (outputStream: StreamReader) (continuation: string list -> string list) =
        let nextLine = outputStream.ReadLine()

        if isNull nextLine then
            continuation []
        else
            readLines outputStream (fun lines -> nextLine :: lines |> continuation)

    readLines outputStream id

let private startProcess (ps: ProcessStartInfo) : Result<Process, ProcessStartError> =
    try
        Ok(Process.Start ps)
    with
    | :? Win32Exception as win32ex ->
        let pathEnv = Environment.GetEnvironmentVariable "PATH"

        Error(
            ProcessStartError.ExecutableFileNotFound(
                ps.FileName,
                ps.Arguments,
                ps.WorkingDirectory,
                pathEnv,
                win32ex.Message
            )
        )
    | ex -> Error(ProcessStartError.UnExpectedException(ps.FileName, ps.Arguments, ex.Message))

let private runToolListCmd (Folder workingDir: Folder) (globalFlag: bool) : Result<string list, DotNetToolListError> =
    let ps = ProcessStartInfo("dotnet")
    ps.WorkingDirectory <- workingDir
    ps.EnvironmentVariables.["DOTNET_CLI_UI_LANGUAGE"] <- "en-us" //ensure we have predictible output for parsing

    let toolArguments = 
        Option.ofObj (Environment.GetEnvironmentVariable "FSHARPLINT_SEARCH_PATH_OVERRIDE")
        |> Option.map(fun env -> $" --tool-path %s{env}")
        |> Option.defaultValue (if globalFlag then "--global" else String.Empty)

    ps.CreateNoWindow <- true
    ps.Arguments <- $"tool list %s{toolArguments}"
    ps.RedirectStandardOutput <- true
    ps.RedirectStandardError <- true
    ps.UseShellExecute <- false

    match startProcess ps with
    | Ok p ->
        p.WaitForExit()
        let exitCode = p.ExitCode

        if exitCode = 0 then
            let output = readOutputStreamAsLines p.StandardOutput
            Ok output
        else
            let error = p.StandardError.ReadToEnd()
            Error(DotNetToolListError.ExitCodeNonZero(ps.FileName, ps.Arguments, exitCode, error))
    | Error err -> Error(DotNetToolListError.ProcessStartError err)

let private (|CompatibleTool|_|) lines =
    let (|HeaderLine|_|) line =
        if Regex.IsMatch(line, @"^Package\sId\s+Version.+$") then
            Some()
        else
            None

    let (|Dashes|_|) line =
        if String.forall ((=) '-') line then Some() else None

    let (|Tools|_|) lines =
        let tools =
            lines
            |> List.choose (fun (line: string) ->
                let parts = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

                if parts.Length > 2 then
                    Some(parts.[0], parts.[1])
                else
                    None)

        if List.isEmpty tools then None else Some tools

    match lines with
    | HeaderLine :: Dashes :: Tools tools ->
        let tool =
            List.tryFind
                (fun (packageId, version) ->
                    match packageId, version with
                    | CompatibleToolName _, CompatibleVersion _ -> true
                    | _ -> false)
                tools

        Option.map (snd >> FSharpLintVersion) tool
    | _ -> None

let private isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

// Find an executable fsharplint file on the PATH
let private fsharpLintVersionOnPath () : (FSharpLintExecutableFile * FSharpLintVersion) option =
    let fsharpLintExecutableOnPathOpt =
        Option.ofObj (Environment.GetEnvironmentVariable("FSHARPLINT_SEARCH_PATH_OVERRIDE"))
        |> Option.orElse (Option.ofObj (Environment.GetEnvironmentVariable("PATH")))
        |> function
        | Some s -> s.Split([| if isWindows then ';' else ':' |], StringSplitOptions.RemoveEmptyEntries)
        | None -> Array.empty
        |> Seq.choose (fun folder ->
            if isWindows then
                let fsharpLintExe = Path.Combine(folder, $"{fsharpLintToolName}.exe")
                if File.Exists fsharpLintExe then Some fsharpLintExe
                else None
            else
                let fsharpLint = Path.Combine(folder, fsharpLintToolName)
                if File.Exists fsharpLint then Some fsharpLint
                else None)
        |> Seq.tryHead

    fsharpLintExecutableOnPathOpt
    |> Option.bind (fun fsharpLintExecutablePath ->
        let processStart = ProcessStartInfo(fsharpLintExecutablePath)
        processStart.Arguments <- "--version"
        processStart.RedirectStandardOutput <- true
        processStart.CreateNoWindow <- true
        processStart.RedirectStandardOutput <- true
        processStart.RedirectStandardError <- true
        processStart.UseShellExecute <- false

        match startProcess processStart with
        | Ok p ->
            p.WaitForExit()
            let stdOut = p.StandardOutput.ReadToEnd()

            stdOut
            |> Option.ofObj
            |> Option.bind (fun s ->
                if s.Contains("Current version: ", StringComparison.CurrentCultureIgnoreCase) then
                    let version = s.ToLowerInvariant().Replace("current version: ", String.Empty).Trim()
                    Some (FSharpLintExecutableFile(fsharpLintExecutablePath), FSharpLintVersion(version))
                else 
                    None)
        | Error(ProcessStartError.ExecutableFileNotFound _)
        | Error(ProcessStartError.UnExpectedException _) -> None)

let findFSharpLintTool (workingDir: Folder) : Result<FSharpLintToolFound, FSharpLintToolError> =
    // First try and find a local tool for the folder.
    // Next see if there is a global tool.
    // Lastly check if an executable is present on the PATH.
    let localToolsListResult = runToolListCmd workingDir false

    match localToolsListResult with
    | Ok(CompatibleTool version) -> Ok(FSharpLintToolFound(version, FSharpLintToolStartInfo.LocalTool workingDir))
    | Error err -> Error(FSharpLintToolError.DotNetListError err)
    | Ok _localToolListResult ->
        let globalToolsListResult = runToolListCmd workingDir true

        match globalToolsListResult with
        | Ok(CompatibleTool version) -> Ok(FSharpLintToolFound(version, FSharpLintToolStartInfo.GlobalTool))
        | Error err -> Error(FSharpLintToolError.DotNetListError err)
        | Ok _nonCompatibleGlobalVersion ->
            let onPathVersion = fsharpLintVersionOnPath ()

            match onPathVersion with
            | Some(executableFile, FSharpLintVersion(CompatibleVersion version)) ->
                Ok(FSharpLintToolFound((FSharpLintVersion(version)), FSharpLintToolStartInfo.ToolOnPath executableFile))
            | _ -> Error FSharpLintToolError.NoCompatibleVersionFound

let createFor (startInfo: FSharpLintToolStartInfo) : Result<RunningFSharpLintTool, ProcessStartError> =
    let processStart =
        match startInfo with
        | FSharpLintToolStartInfo.LocalTool(Folder workingDirectory) ->
            let ps = ProcessStartInfo("dotnet")
            ps.WorkingDirectory <- workingDirectory
            ps.Arguments <- $"{fsharpLintToolName} --daemon"
            ps
        | FSharpLintToolStartInfo.GlobalTool ->
            let userProfile = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)

            let fsharpLintExecutable =
                let fileName = if isWindows then $"{fsharpLintToolName}.exe" else fsharpLintToolName
                Path.Combine(userProfile, ".dotnet", "tools", fileName)

            let ps = ProcessStartInfo(fsharpLintExecutable)
            ps.Arguments <- "--daemon"
            ps
        | FSharpLintToolStartInfo.ToolOnPath(FSharpLintExecutableFile executableFile) ->
            let ps = ProcessStartInfo(executableFile)
            ps.Arguments <- "--daemon"
            ps

    processStart.UseShellExecute <- false
    processStart.RedirectStandardInput <- true
    processStart.RedirectStandardOutput <- true
    processStart.RedirectStandardError <- true
    processStart.CreateNoWindow <- true

    match startProcess processStart with
    | Ok daemonProcess ->
        let handler = new HeaderDelimitedMessageHandler(
            daemonProcess.StandardInput.BaseStream,
            daemonProcess.StandardOutput.BaseStream)
        let client = new JsonRpc(handler)

        do client.StartListening()

        try
            // Get the version first as a sanity check that connection is possible
            let _version =
                client.InvokeAsync<string>(FSharpLint.Client.Contracts.Methods.Version)
                |> Async.AwaitTask
                |> Async.RunSynchronously

            Ok
                { RpcClient = client
                  Process = daemonProcess
                  StartInfo = startInfo }
        with ex ->
            let error =
                if daemonProcess.HasExited then
                    let stdErr = daemonProcess.StandardError.ReadToEnd()
                    $"Daemon std error: {stdErr}.\nJsonRpc exception:{ex.Message}"
                else
                    ex.Message

            Error(ProcessStartError.UnExpectedException(processStart.FileName, processStart.Arguments, error))
    | Error err -> Error err
