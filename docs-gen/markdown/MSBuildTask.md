# Running as MSBuildTask

FSharpLint can be run as an MSBuild task; this will result in lint warnings showing up in your IDE (Visual Studio/Rider).

To set this up, first [install the FSharpLint dotnet tool](ConsoleApplication).

Then, you can add the following to any of your projects to run linting after build completion for that project:

    [lang=xml]
    <Target Name="FSharpLint" AfterTargets="AfterBuild">
     <Exec
       Command="dotnet fsharplint -format msbuild -f $(MSBuildProjectFullPath)"
       ConsoleToMsBuild="true"
       IgnoreExitCode="true"
     />
    </Target>

If you would like to enable linting for all projects, you can add the above target to either a `Directory.Build.props` or `Directory.Build.target` file in the root of your repository. This will add the target to all files. See [here](https://docs.microsoft.com/en-us/visualstudio/msbuild/customize-your-build?view=vs-2019) for more info