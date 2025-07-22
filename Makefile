all:
	@which dotnet > /dev/null || { echo "ERROR: 'dotnet' not found. Please ensure you have installed .NET (the version specified in global.json)" >&2; exit 1; }
	dotnet fsi build.fsx --target Build

check:
	dotnet fsi build.fsx --target Test

selfcheck:
	dotnet fsi build.fsx --target SelfCheck

docs:
	dotnet fsi build.fsx --target Docs

