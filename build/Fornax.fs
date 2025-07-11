namespace Fake.DotNet

open System.IO
open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators

/// <summary>
/// Contains tasks to interact with <a href="https://github.com/ionide/Fornax">Fornax</a> static site generator
/// for F# documentation generation.
/// </summary>
[<RequireQualifiedAccess>]
module Fornax =

    /// <summary>
    /// Fornax build command parameters and options
    /// </summary>
    type BuildParams = {
        /// Working directory to run Fornax from (default: docs directory)
        WorkingDirectory : string option

        /// Timeout for the build process
        Timeout : System.TimeSpan option

        /// Whether to fail if Fornax returns non-zero exit code
        FailOnError : bool
    } with
        /// Parameter default values.
        static member Default = {
            WorkingDirectory = None
            Timeout = None
            FailOnError = true
        }

    /// <summary>
    /// Fornax watch command parameters and options
    /// </summary>
    type WatchParams = {
        /// Working directory to run Fornax from (default: docs directory)
        WorkingDirectory : string option

        /// Port to serve content on (default: 8080)
        Port : int option

        /// Whether to fail if Fornax returns non-zero exit code
        FailOnError : bool
    } with
        /// Parameter default values.
        static member Default = {
            WorkingDirectory = None
            Port = None
            FailOnError = true
        }

    /// <summary>
    /// Build documentation using Fornax
    /// </summary>
    ///
    /// <param name="setBuildParams">Function used to overwrite the build command default parameters.</param>
    ///
    /// <example>
    /// <code lang="fsharp">
    /// Fornax.build (fun p -> { p with WorkingDirectory = Some "./docs" })
    /// </code>
    /// </example>
    let build setBuildParams =
        let buildParams = setBuildParams BuildParams.Default

        let processArgs =
            CreateProcess.fromRawCommandLine "dotnet" "fornax build"
            |> CreateProcess.withTimeout (buildParams.Timeout |> Option.defaultValue (System.TimeSpan.FromMinutes 10.0))
            |> (fun args ->
                match buildParams.WorkingDirectory with
                | Some dir -> CreateProcess.withWorkingDirectory dir args
                | None -> args)
            |> (fun args ->
                if buildParams.FailOnError then
                    CreateProcess.ensureExitCode args
                else
                    args)

        let result = processArgs |> Proc.run

        if buildParams.FailOnError && result.ExitCode <> 0 then
            failwithf "Fornax build failed with exit code %d" result.ExitCode
        else
            result

    /// <summary>
    /// Watch documentation using Fornax with hot reload
    /// </summary>
    ///
    /// <param name="setWatchParams">Function used to overwrite the watch command default parameters.</param>
    ///
    /// <example>
    /// <code lang="fsharp">
    /// Fornax.watch (fun p -> { p with Port = Some 3000; WorkingDirectory = Some "./docs" })
    /// </code>
    /// </example>
    let watch setWatchParams =
        let watchParams = setWatchParams WatchParams.Default

        let args =
            match watchParams.Port with
            | Some port -> $"fornax watch --port {port}"
            | None -> "fornax watch"

        let processArgs =
            CreateProcess.fromRawCommandLine "dotnet" args
            |> (fun args ->
                match watchParams.WorkingDirectory with
                | Some dir -> CreateProcess.withWorkingDirectory dir args
                | None -> args)
            |> (fun args ->
                if watchParams.FailOnError then
                    CreateProcess.ensureExitCode args
                else
                    args)

        let result = processArgs |> Proc.run

        if watchParams.FailOnError && result.ExitCode <> 0 then
            failwithf "Fornax watch failed with exit code %d" result.ExitCode
        else
            result

    /// <summary>
    /// Clean Fornax cache and generated files
    /// </summary>
    ///
    /// <param name="workingDirectory">Working directory where Fornax cache should be cleaned</param>
    let cleanCache workingDirectory =
        let cacheDir = workingDirectory </> "_public"
        let tempDir = workingDirectory </> "_temp"

        if Directory.Exists cacheDir then
            Shell.cleanDir cacheDir
        if Directory.Exists tempDir then
            Shell.cleanDir tempDir
