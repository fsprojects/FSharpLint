module FSharpLint.Client.LSPFSharpLintServiceTypes

open System
open System.Diagnostics
open StreamJsonRpc

type FSharpLintResponseCode =
    | ErrToolNotFound = -5
    | ErrFileNotFound = -4
    | ErrFilePathIsNotAbsolute = -3
    | ErrCancellationWasRequested = -2
    | ErrDaemonCreationFailed = -1
    | OkCurrentDaemonVersion = 0

type FSharpLintVersion = FSharpLintVersion of string
type FSharpLintExecutableFile = FSharpLintExecutableFile of string
type Folder = Folder of path: string

[<RequireQualifiedAccess>]
type FSharpLintToolStartInfo =
    | LocalTool of workingDirectory: Folder
    | GlobalTool
    | ToolOnPath of executableFile: FSharpLintExecutableFile

type RunningFSharpLintTool =
    { Process: Process
      RpcClient: JsonRpc
      StartInfo: FSharpLintToolStartInfo }

    interface IDisposable with
        member this.Dispose() : unit =
            if not this.Process.HasExited then
                this.Process.Kill()

            this.Process.Dispose()
            this.RpcClient.Dispose()

[<RequireQualifiedAccess>]
type ProcessStartError =
    | ExecutableFileNotFound of
        executableFile: string *
        arguments: string *
        workingDirectory: string *
        pathEnvironmentVariable: string *
        error: string
    | UnExpectedException of executableFile: string * arguments: string * error: string

[<RequireQualifiedAccess>]
type DotNetToolListError =
    | ProcessStartError of ProcessStartError
    | ExitCodeNonZero of executableFile: string * arguments: string * exitCode: int * error: string

type FSharpLintToolFound = FSharpLintToolFound of version: FSharpLintVersion * startInfo: FSharpLintToolStartInfo

[<RequireQualifiedAccess>]
type FSharpLintToolError =
    | NoCompatibleVersionFound
    | DotNetListError of DotNetToolListError
