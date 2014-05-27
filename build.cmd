@echo off
cls
if not exist packages\FAKE\tools\Fake.exe (
  .nuget\nuget.exe install FAKE -OutputDirectory packages -ExcludeVersion
)
if not exist tools\Nunit\nunit-console.exe (
  .nuget\nuget.exe install NUnit.Runners -OutputDirectory tools -ExcludeVersion
)
packages\FAKE\tools\FAKE.exe build.fsx %*
pause