module FSharpLint.Client.LSPFSharpLintService

type LSPFSharpLintService =
    interface Contracts.IFSharpLintService

    new: unit -> LSPFSharpLintService
