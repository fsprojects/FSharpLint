module FSharpLint.Client.Contracts

open System
open System.Threading
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module Methods =
    [<Literal>]
    let Version = "fsharplint/version"

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
    interface
        inherit IDisposable

        abstract member VersionAsync: VersionRequest * ?cancellationToken: CancellationToken -> Task<FSharpLintResponse>
    end
