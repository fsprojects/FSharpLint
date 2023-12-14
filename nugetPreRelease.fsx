#r "nuget: Fsdk, Version=0.6.0--date20231213-0703.git-d7a5962"

let args = fsi.CommandLineArgs

Fsdk.Network.GetNugetPrereleaseVersionFromBaseVersion args.[1]
|> System.Console.WriteLine
