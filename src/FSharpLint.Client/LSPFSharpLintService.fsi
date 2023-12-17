module FSharpLint.Client.LSPFSharpLintService

type LSPFSharpLintService =
    interface Contracts.FSharpLintService

    new: unit -> LSPFSharpLintService
