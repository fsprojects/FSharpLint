module FSharpLint.Client.FSharpLintToolLocator

open FSharpLint.Client.LSPFSharpLintServiceTypes

val findFSharpLintTool: workingDir: Folder -> Result<FSharpLintToolFound, FSharpLintToolError>

val createFor: startInfo: FSharpLintToolStartInfo -> Result<RunningFSharpLintTool, ProcessStartError>
