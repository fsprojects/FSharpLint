namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharpLint")>]
[<assembly: AssemblyProductAttribute("FSharpLint")>]
[<assembly: AssemblyDescriptionAttribute("Lint tool for F#.")>]
[<assembly: AssemblyVersionAttribute("0.4.1")>]
[<assembly: AssemblyFileVersionAttribute("0.4.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.4.1"
    let [<Literal>] InformationalVersion = "0.4.1"
