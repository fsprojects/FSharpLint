---
title: Install Dotnet Tool
category: how-to
menu_order: 1
---

# Running fsharplint

The console application is a wrapper around the linter. For basic usage, just run `dnx fsharplint lint <input>`, where `input` can be an fsproj, sln, fs, fsx file, or a string of source code. This will install (if run for the first time) and run fsharplint.

Run `dnx fsharplint --help` for full usage information.

# Installing and running on .NET versions before 10

## Installing as dotnet tool

The linter can be [installed as a dotnet tool](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-tool-install).

Install as a global tool: `dotnet tool install -g fsharplint`.

Install as tool to specific directory: `dotnet tool install --tool-path <my_directory> fsharplint`

## Running the Console Application

If installed as a local tool, run `dotnet fsharplint lint <input>`

If installed as a global tool, run it as `fsharplint lint <input>`.
