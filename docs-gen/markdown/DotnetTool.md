# Installing as dotnet tool

The linter can be [installed as a dotnet tool](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-tool-install).

Install as a global tool: `dotnet tool install -g dotnet-fsharplint`.

Install as tool to specific directory: `dotnet tool install --tool-path <my_directory> dotnet-fsharplint`

## Running the Console Application

The console application is a wrapper around the linter. For basic usage, just run `dotnet fsharplint lint <input>`, where `input` can be an fsproj, sln, fs, fsx file, or a string of source code.

Run `dotnet fsharplint --help` for full usage information.
