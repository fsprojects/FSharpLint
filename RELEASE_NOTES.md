##### 0.4.11 - November 10 2016

* Bug fixed by [@rexcfnghk](https://github.com/rexcfnghk): https://github.com/fsprojects/FSharpLint/issues/189

##### 0.4.10 - October 24 2016

* Updated FSharp.Compiler.Service

##### 0.4.9 - October 8 2016

* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/184

##### 0.4.8 - September 13 2016

* Updated wording of warnings.
* Disabled `RecordFieldNamesMustBePascalCase` naming rule by default.

##### 0.4.7-beta - September 9 2016

* Included FSharp.Core.sigdata and FSharp.Core.optdata in the FSharpLint.MSBuild package.

##### 0.4.6 - August 21 2016

* Bug fixed by [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak): https://github.com/fsprojects/FSharpLint/issues/172

##### 0.4.5 - August 5 2016

* Added missing app.config file to FSharpLint.MSBuild package

##### 0.4.5-beta - August 4 2016

* Run linter in separate AppDomain in MSBuild package so that we can add binding redirects to get the correct version of FSharp.Core

##### 0.4.4 - July 23 2016

* Fixed structure of FSharpLint.MSBuild package

##### 0.4.3 - July 23 2016

* Updated FSharp.Compiler.Service

##### 0.4.2 - July 16 2016

* Fixed name convention bug which warned for DU names inside patterns.

##### 0.4.2-beta - July 14 2016

* Added required project cracker files to FSharpLint.Fake package.

##### 0.4.1-beta - July 09 2016

* Updated FSharp.Compiler.Service
* Brought back configuration manager api.
* Targets FSharp.Core 4.4.0.0

##### 0.4.0-beta - June 26 2016

* Added initial dotnet core support.

##### 0.3.0-beta - June 19 2016

* Improved overall performance of linter.
* Updated default configuration to have opinionated rules off by default.

##### 0.2.7 - September 27 2015

* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/126
* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/125
* Added feature - now able to write a message as a suggestion in hints: https://github.com/fsprojects/FSharpLint/issues/117
* Added feature - nulls can now be matched against in hints: https://github.com/fsprojects/FSharpLint/commit/89050c3bc6020477b3b9d50a4ce541aa09b9a270
* Fixed bug - lambda length warnings would be repeated for each argument: https://github.com/fsprojects/FSharpLint/commit/bc49dd6d58ebbbe8f2cdb4857e011fcaf14d9982
* Enhancement - display operators as symbols in eta reduction suggestions: https://github.com/fsprojects/FSharpLint/commit/1b2f115e71b560ccf8019ef5f14df79f0fd62201
* Enhancement - updated warning messages: https://github.com/fsprojects/FSharpLint/commit/49e579ca35de70902def6bb30835593140be7abd
* Partially fixed bug (fixed when type checking enabled): https://github.com/fsprojects/FSharpLint/issues/113
* Partially fixed bug (fixed when type checking enabled): https://github.com/fsprojects/FSharpLint/issues/109
* Configuration can now be written back to XML.
* Configuration API updated to provide management of configuration files.

##### 0.2.6 - July 18 2015

* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/108
* `Default` property now static: https://github.com/fsprojects/FSharpLint/pull/107

##### 0.2.5 - July 7 2015

* FSharp.Core.dll is now included again

##### 0.2.4 - July 6 2015

* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/101
* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/100
* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/99
* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/103

##### 0.2.3 - July 2 2015

* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/98

##### 0.2.2 - June 9 2015

* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/97 
* XmlDoc rules now configurable to apply to code with certain access levels e.g. private or public. Thanks goes to Jon Hamm for implementing this feature

##### 0.2.1 - May 31 2015

* Included FSharp.Core 
* Fixed bug where hints would accidentally match named parameters and property initialisers

##### 0.2.0 - May 31 2015

* Configuration has been updated to be simpler and verifiable via an XSD.
* Type checking is now optional and off by default to speed up the linting.
* More XML documentation rules have been added thanks to [jhamm](https://github.com/jhamm)
* Files can now be ignored by specifying git ignore like globs in the configuration file.
* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/78
* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/90

##### 0.1.16 - March 11 2015

* Handling of project files is now performed by FSharp.Compiler.Service

##### 0.1.15 - February 08 2015

* Added `Enabled` config option to all analysers.

##### 0.1.14 - January 18 2015

* Added a new rule `CanBeReplacedWithComposition` to the `FSharpLint.FunctionReimplementation` analyser: http://fsprojects.github.io/FSharpLint/FSharpLint.FunctionReimplementation.html
* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/63

##### 0.1.13 - January 11 2015

* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/63
* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/57
* Files can now be ignored via the .fsproj file: https://github.com/fsprojects/FSharpLint/issues/55
* FAKE task now reports on how many files were linted and how many warnings were found.
* FAKE task now includes more detailed information on failure.
* FAKE task now includes an option to fail the build if any warnings are found.

##### 0.1.12 - December 16 2014

* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/57

##### 0.1.11 - November 23 2014

* FSharp.Compiler.Service now included as a strongly named assembly.
* TargetFrameworkVersion now taken from the project file: https://github.com/fsprojects/FSharpLint/issues/52

##### 0.1.10 - November 19 2014

* Dropped MSBuild tools back down from 12 to 4 to support VS2010

##### 0.1.9 - November 17 2014

* Updated `FSharp.Compiler.Service` for compatibility with VisualFSharpPowerTools: https://github.com/fsprojects/FSharpLint/issues/51

##### 0.1.8 - November 15 2014

* FSharp.Core lookup now supports F# 4, the package is now a single click install in VS 2015 preview

##### 0.1.7 - November 14 2014

* Added support for SuppressMessageAttribute for the Typography analyser
* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/48
* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/47
* Attempted to fix assembly resolution issues by including FParsec built against FSharp.Core 4.3.0.0
* Added FAKE task to the nuget package

##### 0.1.6 - October 25 2014

* Added `FSharpLint.Binding.TupleOfWildcards` rule
* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/38

##### 0.1.5 - October 24 2014

* Added support for `SuppressMessageAttribute` for all analysers except `FSharpLint.Typography`
* Added FAKE task, thanks to [@archaeron](https://github.com/archaeron) for this contribution

##### 0.1.4 - October 21 2014

* Fixed bug: https://github.com/fsprojects/FSharpLint/issues/36

##### 0.1.3 - October 16 2014

* Implemented new analyser: `FSharpLint.RaiseWithTooManyArguments`
* Implemented new rule: `FSharpLint.Binding.WildcardNamedWithAsPattern`

##### 0.1.2 - October 14 2014

* More hints and support added for matching if statements, tuples, lists, array, and patterns with hints

##### 0.1.1 - September 28 2014

* Fixed bug: literals in pattern matches were generating lint warnings saying that they should be camel case, whereas they must be pascal case.

##### 0.1.0 - September 25 2014

* First release
