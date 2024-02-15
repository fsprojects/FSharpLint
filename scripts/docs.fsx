#load "common.fsx"
#load "clean.fsx"

open Common

exec "dotnet"  @"fornax build" "docs"
