@echo off
cls

dotnet restore build.proj --verbosity n

dotnet fake run build.fsx