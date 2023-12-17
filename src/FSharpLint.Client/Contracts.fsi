module FSharpLint.Client.Contracts

open System.Threading
open System.Threading.Tasks

module Methods =

    [<Literal>]
    val Version: string = "fsharplint/version"

type VersionRequest =
    {
        FilePath: string
    }

type FSharpLintResult =
    | Content of string

type FSharpLintResponse = { 
    Code: int
    FilePath: string
    Result : FSharpLintResult
}

type FSharpLintService =
    inherit System.IDisposable

    abstract VersionAsync: VersionRequest * ?cancellationToken: CancellationToken -> Task<FSharpLintResponse>
