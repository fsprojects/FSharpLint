module FSharpLint.Client.LSPFSharpLintServiceTypes

type FSharpLintResponseCode =
    | ToolNotFound = 1
    | FileNotFound = 2
    | FilePathIsNotAbsolute = 3
    | CancellationWasRequested = 4
    | DaemonCreationFailed = 5
    | Version = 6

type FSharpLintVersion = FSharpLintVersion of string

type FSharpLintExecutableFile = FSharpLintExecutableFile of string

type Folder = Folder of path: string

[<RequireQualifiedAccess>]
type FSharpLintToolStartInfo =
    | LocalTool of workingDirectory: Folder
    | GlobalTool
    | ToolOnPath of executableFile: FSharpLintExecutableFile

type RunningFSharpLintTool =
    { Process: System.Diagnostics.Process
      RpcClient: StreamJsonRpc.JsonRpc
      StartInfo: FSharpLintToolStartInfo }

    interface System.IDisposable

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
