module FSharpLint.Client.LSPFSharpLintService

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open StreamJsonRpc
open FSharpLint.Client.Contracts
open FSharpLint.Client.LSPFSharpLintServiceTypes
open FSharpLint.Client.FSharpLintToolLocator

type ServiceState =
    { Daemons: Map<FSharpLintVersion, RunningFSharpLintTool>
      FolderToVersion: Map<Folder, FSharpLintVersion> }

    static member Empty: ServiceState =
        { Daemons = Map.empty
          FolderToVersion = Map.empty }

[<RequireQualifiedAccess>]
type GetDaemonError =
    | DotNetToolListError of error: DotNetToolListError
    | FSharpLintProcessStart of error: ProcessStartError
    | InCompatibleVersionFound
    | CompatibleVersionIsKnownButNoDaemonIsRunning of version: FSharpLintVersion

type Msg =
    | GetDaemon of folder: Folder * replyChannel: AsyncReplyChannel<Result<JsonRpc, GetDaemonError>>
    | Reset of AsyncReplyChannel<unit>

let private createAgent (ct: CancellationToken) =
    MailboxProcessor.Start(
        (fun inbox ->
            let rec messageLoop (state: ServiceState) =
                async {
                    let! msg = inbox.Receive()

                    let nextState =
                        match msg with
                        | GetDaemon(folder, replyChannel) ->
                            // get the version for that folder
                            // look in the cache first
                            let versionFromCache = Map.tryFind folder state.FolderToVersion

                            match versionFromCache with
                            | Some version ->
                                let daemon = Map.tryFind version state.Daemons

                                match daemon with
                                | Some daemon ->
                                    // We have a daemon for the required version in the cache, check if we can still use it.
                                    if daemon.Process.HasExited then
                                        // weird situation where the process has crashed.
                                        // Trying to reboot
                                        (daemon :> IDisposable).Dispose()

                                        let newDaemonResult = createFor daemon.StartInfo

                                        match newDaemonResult with
                                        | Ok newDaemon ->
                                            replyChannel.Reply(Ok newDaemon.RpcClient)

                                            { FolderToVersion = Map.add folder version state.FolderToVersion
                                              Daemons = Map.add version newDaemon state.Daemons }
                                        | Error pse ->
                                            replyChannel.Reply(Error(GetDaemonError.FSharpLintProcessStart pse))
                                            state
                                    else
                                        // return running client
                                        replyChannel.Reply(Ok daemon.RpcClient)

                                        { state with
                                            FolderToVersion = Map.add folder version state.FolderToVersion }
                                | None ->
                                    // This is a strange situation, we know what version is linked to that folder but there is no daemon
                                    // The moment a version is added, is also the moment a daemon is re-used or created
                                    replyChannel.Reply(
                                        Error(GetDaemonError.CompatibleVersionIsKnownButNoDaemonIsRunning version)
                                    )

                                    state
                            | None ->
                                // Try and find a version of fsharplint daemon for our current folder
                                let fsharpLintToolResult: Result<FSharpLintToolFound, FSharpLintToolError> =
                                    findFSharpLintTool folder

                                match fsharpLintToolResult with
                                | Ok(FSharpLintToolFound(version, startInfo)) ->
                                    let createDaemonResult = createFor startInfo

                                    match createDaemonResult with
                                    | Ok daemon ->
                                        replyChannel.Reply(Ok daemon.RpcClient)

                                        { Daemons = Map.add version daemon state.Daemons
                                          FolderToVersion = Map.add folder version state.FolderToVersion }
                                    | Error pse ->
                                        replyChannel.Reply(Error(GetDaemonError.FSharpLintProcessStart pse))
                                        state
                                | Error FSharpLintToolError.NoCompatibleVersionFound ->
                                    replyChannel.Reply(Error GetDaemonError.InCompatibleVersionFound)
                                    state
                                | Error(FSharpLintToolError.DotNetListError dotNetToolListError) ->
                                    replyChannel.Reply(Error(GetDaemonError.DotNetToolListError dotNetToolListError))
                                    state
                        | Reset replyChannel ->
                            Map.toList state.Daemons
                            |> List.iter (fun (_, daemon) -> (daemon :> IDisposable).Dispose())

                            replyChannel.Reply()
                            ServiceState.Empty

                    return! messageLoop nextState
                }

            messageLoop ServiceState.Empty),
        cancellationToken = ct
    )

type FSharpLintServiceError =
    | DaemonNotFound of GetDaemonError
    | FileDoesNotExist
    | FilePathIsNotAbsolute
    | CancellationWasRequested

let isPathAbsolute (path: string) : bool =
    if
        String.IsNullOrWhiteSpace path
        || path.IndexOfAny(Path.GetInvalidPathChars()) <> -1
        || not (Path.IsPathRooted path)
    then
        false
    else
        let pathRoot = Path.GetPathRoot path
        // Accepts X:\ and \\UNC\PATH, rejects empty string, \ and X:, but accepts / to support Linux
        if pathRoot.Length <= 2 && pathRoot <> "/" then
            false
        else if pathRoot.[0] <> '\\' || pathRoot.[1] <> '\\' then
            true
        else
            pathRoot.Trim('\\').IndexOf('\\') <> -1 // A UNC server name without a share name (e.g "\\NAME" or "\\NAME\") is invalid

let private isCancellationRequested (requested: bool) : Result<unit, FSharpLintServiceError> =
    if requested then
        Error FSharpLintServiceError.CancellationWasRequested
    else
        Ok()

let private getFolderFor filePath (): Result<Folder, FSharpLintServiceError> =
    let handleFile filePath =
        if not (isPathAbsolute filePath) then
            Error FSharpLintServiceError.FilePathIsNotAbsolute
        else match Folder.from filePath with
             | None -> Error FSharpLintServiceError.FileDoesNotExist
             | Some folder -> Ok folder
    
    handleFile filePath

let private getDaemon (agent: MailboxProcessor<Msg>) (folder: Folder) : Result<JsonRpc, FSharpLintServiceError> =
    let daemon = agent.PostAndReply(fun replyChannel -> GetDaemon(folder, replyChannel))

    match daemon with
    | Ok daemon -> Ok daemon
    | Error gde -> Error(FSharpLintServiceError.DaemonNotFound gde)

let private fileNotFoundResponse filePath : Task<FSharpLintResponse> =
    { Code = int FSharpLintResponseCode.ErrFileNotFound
      FilePath = filePath
      Result = Content $"File \"%s{filePath}\" does not exist."
    }
    |> Task.FromResult

let private fileNotAbsoluteResponse filePath : Task<FSharpLintResponse> =
    { Code = int FSharpLintResponseCode.ErrFilePathIsNotAbsolute
      FilePath = filePath
      Result = Content $"\"%s{filePath}\" is not an absolute file path. Relative paths are not supported."
    }
    |> Task.FromResult

let private daemonNotFoundResponse filePath (error: GetDaemonError) : Task<FSharpLintResponse> =
    let content, code =
        match error with
        | GetDaemonError.DotNetToolListError(DotNetToolListError.ProcessStartError(ProcessStartError.ExecutableFileNotFound(executableFile,
                                                                                                                            arguments,
                                                                                                                            workingDirectory,
                                                                                                                            pathEnvironmentVariable,
                                                                                                                            error)))
        | GetDaemonError.FSharpLintProcessStart(ProcessStartError.ExecutableFileNotFound(executableFile,
                                                                                       arguments,
                                                                                       workingDirectory,
                                                                                       pathEnvironmentVariable,
                                                                                       error)) ->
            $"FSharpLint.Client tried to run `%s{executableFile} %s{arguments}` inside working directory \"{workingDirectory}\" but could not find \"%s{executableFile}\" on the PATH (%s{pathEnvironmentVariable}). Error: %s{error}",
            FSharpLintResponseCode.ErrDaemonCreationFailed
        | GetDaemonError.DotNetToolListError(DotNetToolListError.ProcessStartError(ProcessStartError.UnexpectedException(executableFile,
                                                                                                                         arguments,
                                                                                                                         error)))
        | GetDaemonError.FSharpLintProcessStart(ProcessStartError.UnexpectedException(executableFile, arguments, error)) ->
            $"FSharpLint.Client tried to run `%s{executableFile} %s{arguments}` but failed with \"%s{error}\"",
            FSharpLintResponseCode.ErrDaemonCreationFailed
        | GetDaemonError.DotNetToolListError(DotNetToolListError.ExitCodeNonZero(executableFile,
                                                                                 arguments,
                                                                                 exitCode,
                                                                                 error)) ->
            $"FSharpLint.Client tried to run `%s{executableFile} %s{arguments}` but exited with code {exitCode} {error}",
            FSharpLintResponseCode.ErrDaemonCreationFailed
        | GetDaemonError.InCompatibleVersionFound ->
            "FSharpLint.Client did not found a compatible dotnet tool version to launch as daemon process",
            FSharpLintResponseCode.ErrToolNotFound
        | GetDaemonError.CompatibleVersionIsKnownButNoDaemonIsRunning(FSharpLintVersion version) ->
            $"FSharpLint.Client found a compatible version `%s{version}` but no daemon could be launched.",
            FSharpLintResponseCode.ErrDaemonCreationFailed

    { Code = int code
      FilePath = filePath
      Result = Content content
    }
    |> Task.FromResult

let private cancellationWasRequestedResponse filePath : Task<FSharpLintResponse> =
    { Code = int FSharpLintResponseCode.ErrCancellationWasRequested
      FilePath = filePath
      Result = Content "FSharpLintService is being or has been disposed."
    }
    |> Task.FromResult

let mapResultToResponse (filePath: string) (result: Result<Task<FSharpLintResponse>, FSharpLintServiceError>) =
    match result with
    | Ok t -> t
    | Error FSharpLintServiceError.FileDoesNotExist -> fileNotFoundResponse filePath
    | Error FSharpLintServiceError.FilePathIsNotAbsolute -> fileNotAbsoluteResponse filePath
    | Error(FSharpLintServiceError.DaemonNotFound e) -> daemonNotFoundResponse filePath e
    | Error FSharpLintServiceError.CancellationWasRequested -> cancellationWasRequestedResponse filePath

type LSPFSharpLintService() =
    let cts = new CancellationTokenSource()
    let agent = createAgent cts.Token

    interface FSharpLintService with
        member this.Dispose() =
            if not cts.IsCancellationRequested then
                agent.PostAndReply Reset |> ignore
                cts.Cancel()

        member _.VersionAsync(versionRequest: VersionRequest, ?cancellationToken: CancellationToken) : Task<FSharpLintResponse> =
            isCancellationRequested cts.IsCancellationRequested
            |> Result.bind (getFolderFor (versionRequest.FilePath))
            |> Result.bind (getDaemon agent)
            |> Result.map (fun client ->
                client
                    .InvokeWithCancellationAsync<string>(
                        Methods.Version,
                        cancellationToken = Option.defaultValue cts.Token cancellationToken
                    )
                    .ContinueWith(fun (t: Task<string>) ->
                        { Code = int FSharpLintResponseCode.OkCurrentDaemonVersion
                          Result = Content t.Result
                          FilePath = versionRequest.FilePath }))
            |> mapResultToResponse versionRequest.FilePath
