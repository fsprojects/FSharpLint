#### Running the MSBuild Task

Install the [package via nuget](https://www.nuget.org/packages/FSharpLint.MSBuild/), the nuget package 
contains a targets file which runs the linter as an `AfterBuild` task. Once installed, building the project
will run the linter. 

The following is what nuget will add to your project's `.fsproj` file:

	[lang=xml]
    <Import Project="$(SolutionDir)\packages\FSharpLint.MSBuild\build\FSharpLint.MSBuild.targets" />

The lint task provides a way to run the linter and get warnings in your IDE without having to install any plugins.
It's not recommended to keep this as part of your regular build process, it will significantly slow down build times.

#### Example In Use

![Example of MSBuild task in use](http://i.imgur.com/D4c9g1m.png)