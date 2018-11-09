#!/usr/bin/env bash

set -eu
set -o pipefail

dotnet restore build.proj --verbosity n

dotnet fake run build.fsx