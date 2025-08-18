all:
	@which dotnet > /dev/null || { echo "ERROR: 'dotnet' not found. Please ensure you have installed .NET (the version specified in global.json)" >&2; exit 1; }
	dotnet fsi build.fsx --target Build

check:
	dotnet fsi build.fsx --target Test

selfcheck:
	dotnet fsi build.fsx --target SelfCheck

# added docs tatget to .PHONY target because, otherwise, as 'docs' dir already exists
# then Make would consider it "up to date" and would skip execution of the target
.PHONY: docs
docs:
	$(MAKE) --directory docs

