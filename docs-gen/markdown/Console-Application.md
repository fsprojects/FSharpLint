#### Running the Console Application

The console application is a wrapper around the linter, with a required argument of: `-f`, `-sf`, or `-source`. 

* `-f` lints an entire project.
* `-sf` lints a single file.
* `-source` lints a string.

##### Example

`C:\FSharpLint\bin` is the directory containing `fsharplint.exe`

`C:\FSharpLint.FunctionalTest.TestedProject\FSharpLint.FunctionalTest.TestedProject.fsproj` is the path of the project file of the project that we want to lint.

To run the linter, open the command line, `cd` into the directory containing `fsharplint.exe` and run the tool using the `-f` argument to specify the project file:

    >cd C:\FSharpLint\bin
    >fsharplint -f "C:\FSharpLint.FunctionalTest.TestedProject\FSharpLint.FunctionalTest.TestedProject.fsproj"

###### Output

![Example run](http://i.imgur.com/0DZTdDR.png "Example run of the console application")

#### Application Arguments

| Arguments | Description | Example |
| --- | :------------- | --- |
| `-f` | Path to the project file. | `-f "C:\FSharpProjectDirectory\ProjectFile.fsproj"` |
| `-sf` | Path to a source file. | `-f "C:\FSharpProjectDirectory\Uploader.fsx"` |
| `-source` | String to be linted. | `-source "let x = fun _ -> ()"` |