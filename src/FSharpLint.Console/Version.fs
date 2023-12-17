[<RequireQualifiedAccess>]
module FSharpLint.Console.Version

open System.Reflection

let get () = 
    Assembly.GetExecutingAssembly().GetCustomAttributes false
    |> Seq.pick (function | :? AssemblyInformationalVersionAttribute as aiva -> Some aiva.InformationalVersion | _ -> None)
