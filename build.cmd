@echo off
cls
if not exist packages\FAKE\tools\Fake.exe (
  .nuget\nuget.exe install FAKE -OutputDirectory packages -ExcludeVersion
)
if not exist tools\NUnit.Runners\tools\nunit-console.exe (
  .nuget\nuget.exe install NUnit.Runners -OutputDirectory tools -ExcludeVersion
)
if not exist tools\FSharpLint.0.2.0\FSharpLint.FAKE.dll (
  .nuget\nuget.exe install FSharpLint -OutputDirectory tools -Version 0.2.0
)
packages\FAKE\tools\FAKE.exe build.fsx %*