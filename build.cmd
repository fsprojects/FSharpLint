@echo off
cls
if not exist packages\FAKE\tools\Fake.exe (
  .nuget\nuget.exe install FAKE -OutputDirectory packages -ExcludeVersion
)
if not exist tools\NUnit.Runners\tools\nunit-console.exe (
  .nuget\nuget.exe install NUnit.Runners -OutputDirectory tools -ExcludeVersion
)
if not exist tools\FSharpLint.0.2.4\FSharpLint.FAKE.dll (
  .nuget\nuget.exe install FSharpLint -OutputDirectory tools -Version 0.2.4
)
if not exist tools\FSharp.Formatting\FSharp.Formatting.fsx (
  .nuget\nuget.exe install FSharp.Formatting -OutputDirectory tools -ExcludeVersion
)
packages\FAKE\tools\FAKE.exe build.fsx %*