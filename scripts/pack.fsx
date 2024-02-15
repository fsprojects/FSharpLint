#r "nuget: Fake.DotNet.Cli"
#load "common.fsx"
#load "default.fsx"

open Fake.DotNet

open Common

let properties = PackageReleaseNotes ([
    ("Version", nugetVersion);
    ("Authors", authors)
    ("PackageProjectUrl", gitUrl)
    ("RepositoryType", "git")
    ("RepositoryUrl", gitUrl)
    ("PackageLicenseExpression", "MIT")
])

DotNet.pack (fun p ->
    { p with
        Configuration = DotNet.BuildConfiguration.Release
        OutputPath = Some nugetDir
        MSBuildParams = { p.MSBuildParams with Properties = properties }
    }
) "FSharpLint.sln"
