#### Using the Console Application

The console application is a very simple wrapper around the lint tool, as of now it has a single argument `-f` which is required to specify the path to the project file of the project that you want to run the tool against.

##### Example

`C:\FSharpLint\bin` is the directory containing `fsharplint.exe`

`C:\FSharpLint.FunctionalTest.TestedProject\FSharpLint.FunctionalTest.TestedProject.fsproj` is the path of the project file of the project that we want to lint.

To run the tool we need to open the command line, `cd` into the directory containing `fsharplint.exe` and then run the tool using the `-f` argument to specify the project file:

    >cd C:\FSharpLint\bin
    >fsharplint -f "C:\FSharpLint.FunctionalTest.TestedProject\FSharpLint.FunctionalTest.TestedProject.fsproj"

###### Output

![Example run](http://i.imgur.com/0DZTdDR.png "Example run of the console application")

#### Application Arguments

| Arguments | Description | Example |
| --- | :------------- | --- |
| `-f` | Specifies the path to the project file of the project to be linted. **(Required)** | `fsharplint -f "C:\FSharpProjectDirectory\ProjectFile.fsproj"` |

#### Future Improvements

* Optional argument to the F# compiler directory needs to be added.
* Optional argument to a config file needs to be added.