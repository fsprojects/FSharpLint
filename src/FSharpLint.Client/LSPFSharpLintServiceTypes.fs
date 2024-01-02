module FSharpLint.Client.LSPFSharpLintServiceTypes

open System
open System.Diagnostics
open System.IO
open StreamJsonRpc

type FSharpLintResponseCode =
    | ErrToolNotFound = -5
    | ErrFileNotFound = -4
    | ErrFilePathIsNotAbsolute = -3
    | ErrCancellationWasRequested = -2
    | ErrDaemonCreationFailed = -1
    | OkCurrentDaemonVersion = 0
    | OkLint = 1
    | OkLintError = 2

type File = private File of string
with
    static member From (filePath: string) =
        if File.Exists(filePath) then
            filePath |> File |> Some
        else
            None

    static member Unwrap(File file) = file

type FSharpLintVersion = FSharpLintVersion of string
type FSharpLintExecutableFile = FSharpLintExecutableFile of File
type Folder = private Folder of string
with
    static member FromFile (filePath: string) =
        if File.Exists(filePath) then
            let folder = (FileInfo filePath).Directory
            if folder.Exists then
                folder.FullName |> Folder |> Some
            else
                None
        else
            None
    static member FromFolder (folderPath: string) =
        if Directory.Exists(folderPath) then
            let folder = DirectoryInfo folderPath
            folder.FullName |> Folder |> Some
        else
            None
    static member Unwrap(Folder folder) = folder

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
    | UnexpectedException of executableFile: string * arguments: string * error: string

[<RequireQualifiedAccess>]
type DotNetToolListError =
    | ProcessStartError of ProcessStartError
    | ExitCodeNonZero of executableFile: string * arguments: string * exitCode: int * error: string

type FSharpLintToolFound = FSharpLintToolFound of version: FSharpLintVersion * startInfo: FSharpLintToolStartInfo

[<RequireQualifiedAccess>]
type FSharpLintToolError =
    | NoCompatibleVersionFound
    | DotNetListError of DotNetToolListError
