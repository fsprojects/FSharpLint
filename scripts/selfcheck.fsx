#load "common.fsx"

open System.IO

open Common

let srcDir = Path.Combine(rootDir.FullName, "src") |> DirectoryInfo

let consoleProj = Path.Combine(srcDir.FullName, "FSharpLint.Console", "FSharpLint.Console.fsproj") |> FileInfo
let sol = Path.Combine(rootDir.FullName, "FSharpLint.sln") |> FileInfo
exec "dotnet" (sprintf "run lint %s" sol.FullName) consoleProj.Directory.FullName
