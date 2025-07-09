#!/usr/bin/env bash

set -eu
set -o pipefail

FAKE_DETAILED_ERRORS=true dotnet run --project ./build/build.fsproj -- --target "$@"
