all:
	which dotnet > /dev/null || { echo "ERROR: dotnet not found. Please ensure you have installed the .NET SDK (v6 or higher)" >&2; exit 1; }
	dotnet fake build --target Build

check:
	dotnet fake build --target Test
