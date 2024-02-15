#r "nuget: Fake.IO.FileSystem"
#r "nuget: Fake.Core.Process"
#r "nuget: Fake.Core.ReleaseNotes"
#load "common.fsx"

open Fake.IO

open Common

Shell.cleanDirs [ buildDir; nugetDir ]
