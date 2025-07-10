$ErrorActionPreference = 'Stop'

# Pass all arguments to dotnet run
$env:FAKE_DETAILED_ERRORS = 'true'
dotnet run --project ./build/build.fsproj -- --target $args
