#!/usr/bin/env bash

set -eu
set -o pipefail

cd `dirname $0`

dotnet restore build.proj --verbosity n

dotnet fake run build.fsx