#### Including The Task In Your Project's Project File

The task must be declared with the `UsingTask` element as follows:

    <UsingTask AssemblyFile="C:\FSharpLint\bin\FSharpLint.MSBuildIntegration.dll" TaskName="FSharpLintTask" />

The `AssemblyFile` property must be the path to the `FSharpLint.MSBuildIntegration.dll` assembly, this file should have been placed into the `/src/FSharpLint.MSBuildIntegration/bin/Release/` directory when building the tool.

The task has a required property `Project` which must be a path to the project file, use `$(MSBuildProjectFullPath)` for this since it's a reference to the current file which is what we want:

	[lang=xml]
    <Target Name="AfterBuild">
        <FSharpLintTask Project="$(MSBuildProjectFullPath)" />
    </Target>

The task has an optional property `TreatWarningsAsErrors` which will fail the build if there are any lint errors by displaying them as build errors:

	[lang=xml]
    <Target Name="AfterBuild">
        <FSharpLintTask Project="$(MSBuildProjectFullPath)" TreatWarningsAsErrors="true" />
    </Target>

The following should be in your `.fsproj` file, with the `AssemblyFile` path ammended:

	[lang=xml]
    <UsingTask AssemblyFile="C:\FSharpLint\bin\FSharpLint.MSBuildIntegration.dll" TaskName="FSharpLintTask" />
    <Target Name="AfterBuild">
        <FSharpLintTask Project="$(MSBuildProjectFullPath)" />
    </Target>

#### Example In Use

![Example of MSBuild task in use](http://i.imgur.com/D4c9g1m.png)