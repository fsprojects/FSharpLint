@echo off
cls

# Restore the tool NuGet packages
dotnet tool restore

dotnet restore build.proj --verbosity n

dotnet fake run build.fsx