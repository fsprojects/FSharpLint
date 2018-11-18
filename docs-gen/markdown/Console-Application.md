#### Installing the Console Application

The linter can be [installed as a dotnet tool](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-tool-install).

Install as a global tool: `dotnet install -g dotnet-fsharplint`.

Install as tool to specific directory: `dotnet install --tool-path <my_directory> dotnet-fsharplint`

#### Running the Console Application

The console application is a wrapper around the linter, with a required argument of: `-f`, `-sf`, or `-source`. 

| Arguments | Description | Example |
| --- | :------------- | --- |
| `-f <project_file_path>` | Lints an entire project. | `dotnet fsharplint -f "C:\FSharpProjectDirectory\ProjectFile.fsproj"` |
| `-sf <source_file_path>` | Lints a single file. | `dotnet fsharplint -sf "C:\FSharpProjectDirectory\Uploader.fsx"` |
| `-source <source_string>` | Lints a string. | `dotnet fsharplint -source "let x = fun _ -> ()"` |
