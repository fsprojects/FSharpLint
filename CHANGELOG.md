# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.24.2] - 2024-02-29

- New rule EnsureTailCallDiagnosticsInRecursiveFunctions #673 #651 [@webwarrior-ws]
- Fix AvoidSinglePipeOperator false negatives #697 [@Mersho]
- Fix AvoidSinglePipeOperator false negative #696 [@Mersho]
- Fix FavourStaticEmptyFields false negative #695 [@Mersho]
- Benchmarks: stop forcing .NETCore2.1 runtime #694 #620 [@Numpsy]
- paket: upgrade Ionide.ProjInfo to 0.61.3 #688 [@webwarrior-ws]
- Fix FavourStaticEmptyFields false negative #689 [@Mersho]
- build: change NoWarn to WarningsNotAsErrors #691 [@webwarrior-ws]
- New rule FavourNonMutablePropertyInitialization #683 #662 #535 [@webwarrior-ws]
- Workaround for NoPartialFuncs bug #682 [@webwarrior-ws]
- Fix AvoidSinglePipeOperator false positive #684 [@Mersho]
- docs(CSS): capitalize <h1> rather than uppercase [@knocte]
- Simplify SelfCheck #679 [@knocte]


## [0.24.0] - 2024-01-13

Happy 10 year anniversary to FSharpLint!
- Fix error linting projects that use .NET Legacy Framework #336 #657 [@su8898] & [@webwarrior-ws]
- FSharpLint.Console: show URL for each rule at summary #666 [@knocte]
- AvoidTooShortNames: fix for DU member params & lambda arguments #645 [@Mersho]
- Docs: don't swallow errors #678 [@webwarrior-ws]
- Make inline source filePath less confusing #677 #674 [@webwarrior-ws]
- Add new rule UnnecessaryRecKeyword #650 #652 #671 [@Mersho] & [@webwarrior-ws]
- TypePrefixing: new modes "Always", "Hybrid", "Never" (default: Hybrid) #661 [@knocte] & [@webwarrior-ws]
- Less hardcoding of .NET versions #659 [@knocte]


## [0.23.6] - 2024-01-05

- Fix recent regression in MaxLinesIn* rules #667 [@webwarrior-ws] & [@Mersho]
- Migrate from .NET5 to .NET6 #655 #606 #604 [@Numpsy] & [@knocte] & [@webwarrior-ws]
- CI: better separation of main lane steps #654 [@knocte]


## [0.23.0] - 2023-12-28

- A new rule that deters from using underscore-prefixed elements #643 #573 #591 [@tehraninasab] & [@webwarrior-ws]
- CI: don't push pre-releases for release notes commits. #647 [@knocte]
- Ignore empty lines and comments when counting number of lines #644 #634 [@Mersho] & [@webwarrior-ws]


## [0.21.10] - 2023-12-20

- AvoidSinglePipeOperator: fix false positive #640 [@webwarrior-ws]
- CI: improve preRelease version numbers #642 [@knocte]
- AvoidTooShortNames: fix false negative #639 [@Mersho] & [@webwarrior-ws]
- CI: faster publish&docs pipelines thanks to Linux #641 [@knocte]


## [0.21.8] - 2023-12-19

- Add new rules NestedFunctionNames and UnnestedFunctionNames #564 [@su8898] & [@webwarrior-ws]
- Addressed regression in the docs CI deploy [@knocte]
- Move docs CI to main CI (to detect problems earlier next time) #636 [@knocte]


## [0.21.7] - 2023-12-16

- Add new rule SuggestUseAutoProperty #625 #596 [@webwarrior-ws]
- AvoidTooShortNames: fix false negatives and improve warning positions #633 #632 [@Mersho] & [@webwarrior-ws]
- FavourStaticEmptyFields: properly advise Array.empty instead of List.Empty #631 #630 [@webwarrior-ws]
- CI: publish prerelease nuget versions for every commit #629 [@knocte]
- CI: run FSharpLint on itself #609 #628 [@Mersho]


## [0.21.6] - 2023-12-07

- Add new rule AsyncExceptionWithoutReturn #623 #597 [@Mersho]
- FailwithBadUsage: extern declarations bugfix #562 #561 [@webwarrior-ws]
- HintMatcher: fix potential crash #608 #341 [@Thorium]
- CI: upgrade the 'setup-dotnet' action #614 [@Numpsy]
- Additional improvements to docs and CI [@knocte]


## [0.21.5] - 2023-12-06

- FSharpLint.Console: add --version #612 #611 [@Thorium]
- gitignore: add the BenchmarkDotNet artifacts folder #619 [@Numpsy]
- CI: remove redundant .NET install #624 [@knocte]
- Add new rule AvoidSinglePipeOperator #595 [@tehraninasab]
- AvoidTooShortNames: fix false negative (generic type names) #622 [@Mersho]
- Remove recursive dependency #569 [@knocte]
- Conventions/Naming: refactoring #571 [@webwarrior-ws]


## [0.21.4] - 2023-11-23

- Paket: use Packages storage #615 [@Numpsy]
- Fix VS2022 build #613 [@Thorium]
- Dotnet version SDK to allow minor version difference fwd #607 [@Thorium]
- docs: fix Shortcuts typo in the menu #584 [@pirrmann]
- Fix false negatives in AvoidTooShortNames #548 [@janus]
- Fix cyclomatic complexity yielding redundant messages #559 #579 [@davidtgillard]
- Refactoring: rename DU #565 [@webwarrior-ws]

## [0.21.3] - 2022-09-29

- Add new rule FavourStaticEmptyFields #530 [@janus]
- Fix false negative in GenericTypesNames #551 #552 [@janus]
- Fix GenericTypesNames not honoring configuation #552 [@janus]
- Stop ignoring errors when parsing fsharplint.json #550 #553 [@janus]
- Fix false negative in RedundantNewKeyword #555 #556 [@janus]

## [0.21.2] - 2022-04-07

- Fix for false positive in FailwithBadUsage #539 [@janus]
- FavourConsistentThis: fix some false positives and add unit tests for them [@janus]
- NoPartialFunctions: detect instance members Option.Value, Map.Item, List.Head & List.Item [@su8898]
- Cover more scenarios in FailwithBadUsage [@janus]

## [0.21.1] - 2022-03-01

- Add new rule AvoidTooShortNames [@janus]
- Add missing config setting hooks for recently added rules [@janus]

## [0.21.0] - 2021-11-18

- Add new rule FailwithBadUsage [@su8898]
- Add new rule FavourReRaise [@su8898]
- Enable running tool on .Net 6 [@Cheroukee]

## [0.20.2] - 2021-10-09

- Added cyclomatic complexity rule [@davidtgillard]
- Added FavourTypedIgnore rule [@su8898]
- Added GenericTypesNames rule [@su8898]

## [0.20.1] - 2021-09-06

- Fix NonPublicValues' setting backwards compatiblity. [@aarani]

## [0.20.0] - 2021-09-02

- Separate non public values naming rule into two rules (internal and private). [@knocte] & [@su8898]

## [0.19.2] - 2021-07-05

- Fix for extern function naming #487
- Doc update - configuration file info.

## [0.19.1] - 2021-06-23

- Update to FCS 40 [@baronfel]

## [0.19.0] - 2021-06-12

- Update to FCS 39 [@baronfel]
  - Consumers of the API will now need to call `Ionide.ProjInfo.Init.init()`, see the FSharpLint.Console project within this repo for an example.

## [0.18.1] - 2021-01-27

- Update docs.

## [0.18.0] - 2021-01-08

- Added new rule `NoPartialFunctions (FL0066)`.

## [0.17.1] - 2020-11-25
- Fix for records being counted as a nested statement #464 [@davidtgillard]

## [0.17.0] - 2020-11-13
- Update to FCS 38 [@baronfel]
- Update to .Net 5 [@baronfel]

## [0.16.5] - 2020-06-26
- Update navigation in docs.

## [0.16.4] - 2020-06-26
- Update docs to fix broken links.

## [0.16.3] - 2020-06-13
- Update docs to support https.

## [0.16.2] - 2020-06-10
- Load config from `fsharplint.json` by default.

## [0.16.1] - 2020-06-10
- Fix RecordFieldNames rule incorrectly checking Union Case fields

## [0.16.0] - 2020-06-10
- Use `Dotnet.ProjInfo.Workspace` to load project info.

## [0.15.0] - 2020-06-08
- Update FCS to 36.0.1 [@baronfel]
- Check record fields in TypedItemSpacing rule
- Fixes for issues in Indentation rule

## [0.14.2] - 2020-05-20
- Fixes for exceptional indentation cases

## [0.14.1] - 2020-05-19
- .NET Core 3.1 support [@milbrandt]
- Performance optimizations
- Handle exceptional cases for indentation rule
- Fix issue with active pattern naming rule running against measure types

## [0.14.0] - 2020-04-11
- Update FCS to 35 [@baronfel]

## [0.13.3] - 2020-02-25
- Fix for suppression comment parsing

## [0.13.2] - 2020-02-25
- Remove support for multiple overriding configs
- Update FCS to 34.0.1 [@baronfel]

## [0.13.1] - 2020-02-24
- Use structured comments for warning suppression
- Use Argu library for command line argument parsing

## [0.13.0] - 2020-02-20
- Add `-format` flag to specify output format (standard or MSBuild).

## [0.12.10] - 2020-01-23
- Return non-zero error code when there are lint warnings

## [0.12.9] - 2020-01-23
- Fix linting of solution in non-Windows systems

## [0.12.8] - 2020-01-23
- Pass release configuration to dotnet proj info

## [0.12.7] - 2020-01-07
- Add `-c` flag for specifying MSBuild release configuration

## [0.12.6] - 2019-12-05
- Update FCS to 33.0 [@baronfel]

## [0.12.5] - 2019-10-07
- Addressed issue: <https://github.com/fsprojects/FSharpLint/issues/367> [@jrr]

## [0.12.4] - 2019-10-05
- Update FCS to 32.0 [@Krzysztof-Cieslak]

## [0.12.3] - 2019-08-15
- Update FCS to 31.0 [@baronfel]

## [0.12.2] - 2019-07-03
- Add API to convert XmlConfiguration to new config type [@milbrandt]
- Ignore active patterns in PublicValues naming conventions rule
- Update FCS to 30.0 [@baronfel]
- Use Newtonsoft.Json for config parsing

## [0.12.1] - 2019-05-31
- Implement linting of all projects in solution using `-sol` flag, or programmatically using `lintSolution` function
- Return non-zero exit code if lint warnings exist
- Fix bug in converting XML config with no hints defined
- Add `XmlConfiguration.tryLoadConfigurationForProject` to support backwards compatability in external applications

## [0.12.0] - 2019-05-29
- Update `FSharp.Compiler.Service`

## [0.11.1] - 2019-05-14
- Fix issue in loading default configuration

## [0.11.0] - 2019-05-14
- Add ability to disable previously defined hints
- Ignore members implementing interface when checking member naming
- Change config from XML to JSON
- Refactor and redesign linter internals

## [0.10.8] - 2019-04-01
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/323,> by jgardella
- Update `FSharp.Compiler.Service`, by enricosada

## [0.10.7] - 2019-02-26
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/304>

## [0.10.6] - 2019-02-26
- Update `FSharp.Compiler.Service`

## [0.10.5] - 2019-02-13
- Update `FSharp.Compiler.Service`, by baronfel

## [0.10.4] - 2019-02-11
- Improve tuple item spacing check, by jgardella
- Ignore pattern matching in function parameters, by jgardella
- Fix using FSharpLint.Core targeting `net`, by enricosada
- Fix match indentation calculation, by jgardella
- Produce tuple type string correctly, by jgardella
- Fix false positive for tuple instantiation with cons operator, by jgardella
- Take comments into account when checking spacing, by jgardella

## [0.10.3] - 2019-01-29
- Update configuration defaults to exclude formatting rules for now.

## [0.10.2] - 2018-12-20
- API C# interop, thanks to [@jgardella](https://github.com/jgardella)
- Fix guard indentation in FormattingMatchExpressionIndentation rule, thanks to [@jgardella](https://github.com/jgardella)
- Command line interface improvements.

## [0.10.1] - 2018-10-18
- Pack console application as tool.

## [0.10.0] - 2018-10-07
- Move solution to dotnet core.

## [0.9.1-beta] - 2018-02-22
- Fixed <https://github.com/fsprojects/FSharpLint/issues/256> by [@SteveGilham](https://github.com/SteveGilham)
- Fixed <https://github.com/fsprojects/FSharpLint/issues/252> by [@SteveGilham](https://github.com/SteveGilham)

## [0.9.0] - 2018-01-28
- .net standard 2.0 support, thanks to [@enricosada](https://github.com/enricosada)

## [0.9.0-beta] - 2017-10-19
- .net standard 2.0 support, thanks to [@enricosada](https://github.com/enricosada)

## [0.8.1] - 2017-10-10
- Fixed <https://github.com/fsprojects/FSharpLint/issues/240>

## [0.8.0] - 2017-09-05
- Updated `FSharp.Compiler.Service`

## [0.7.7] - 2017-08-20
- Fixed <https://github.com/fsprojects/FSharpLint/issues/237>

## [0.7.6] - 2017-07-02
- Fixed <https://github.com/fsprojects/FSharpLint/issues/229>
- Fixed <https://github.com/fsprojects/FSharpLint/issues/227>
- Updated `FSharp.Compiler.Service`

## [0.7.5-beta] - 2017-03-31
- Updated `FSharp.Compiler.Service`

## [0.7.4-beta] - 2017-03-03
- Updated `FSharp.Compiler.Service`

## [0.7.3-beta] - 2017-02-23
- Updated `FSharp.Compiler.Service`

## [0.7.2-beta] - 2017-02-23
- Updated `FSharp.Compiler.Service`

## [0.7.1-beta] - 2017-02-20
- Added suggestion for redundant usages of the `new` keyword.

## [0.7.0-beta] - 2017-02-12
- Type checks performed at end of lint in parallel
- Linter now cancellable.

## [0.6.5-beta] - 2017-02-11
- Updated `FSharp.Compiler.Service`
- New hints by [@ErikSchierboom](https://github.com/ErikSchierboom)

## [0.6.4-beta] - 2017-01-30
- Updated `FSharp.Compiler.Service`

## [0.6.3-beta] - 2017-01-22
- Improved performance of naming analyser.

## [0.6.2-beta] - 2017-01-21
- Added suggested fixes for naming rules.
- New hint by [@smoothdeveloper](https://github.com/smoothdeveloper): <https://github.com/fsprojects/FSharpLint/pull/207>

## [0.6.1-beta] - 2017-01-17
- Naming rules now customisable thanks to [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak).

## [0.5.1-beta] - 2016-12-31
- Introduced automated fix information to API.

## [0.4.12] - 2016-11-16
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/191>

## [0.4.11] - 2016-11-10
- Bug fixed by [@rexcfnghk](https://github.com/rexcfnghk): <https://github.com/fsprojects/FSharpLint/issues/189>

## [0.4.10] - 2016-10-24
- Updated FSharp.Compiler.Service

## [0.4.9] - 2016-10-08
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/184>

## [0.4.8] - 2016-09-13
- Updated wording of warnings.
- Disabled `RecordFieldNamesMustBePascalCase` naming rule by default.

## [0.4.7-beta] - 2016-09-09
- Included FSharp.Core.sigdata and FSharp.Core.optdata in the FSharpLint.MSBuild package.

## [0.4.6] - 2016-08-21
- Bug fixed by [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak): <https://github.com/fsprojects/FSharpLint/issues/172>

## [0.4.5] - 2016-08-05
- Added missing app.config file to FSharpLint.MSBuild package

## [0.4.5-beta] - 2016-08-04
- Run linter in separate AppDomain in MSBuild package so that we can add binding redirects to get the correct version of FSharp.Core

## [0.4.4] - 2016-07-23
- Fixed structure of FSharpLint.MSBuild package

## [0.4.3] - 2016-07-23
- Updated FSharp.Compiler.Service

## [0.4.2] - 2016-07-16
- Fixed name convention bug which warned for DU names inside patterns.

## [0.4.2-beta] - 2016-07-14
- Added required project cracker files to FSharpLint.Fake package.

## [0.4.1-beta] - 2016-07-09
- Updated FSharp.Compiler.Service
- Brought back configuration manager api.
- Targets FSharp.Core 4.4.0.0

## [0.4.0-beta] - 2016-06-26
- Added initial dotnet core support.

## [0.3.0-beta] - 2016-06-19
- Improved overall performance of linter.
- Updated default configuration to have opinionated rules off by default.

## [0.2.7] - 2015-09-27
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/126>
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/125>
- Added feature - now able to write a message as a suggestion in hints: <https://github.com/fsprojects/FSharpLint/issues/117>
- Added feature - nulls can now be matched against in hints: <https://github.com/fsprojects/FSharpLint/commit/89050c3bc6020477b3b9d50a4ce541aa09b9a270>
- Fixed bug - lambda length warnings would be repeated for each argument: <https://github.com/fsprojects/FSharpLint/commit/bc49dd6d58ebbbe8f2cdb4857e011fcaf14d9982>
- Enhancement - display operators as symbols in eta reduction suggestions: <https://github.com/fsprojects/FSharpLint/commit/1b2f115e71b560ccf8019ef5f14df79f0fd62201>
- Enhancement - updated warning messages: <https://github.com/fsprojects/FSharpLint/commit/49e579ca35de70902def6bb30835593140be7abd>
- Partially fixed bug (fixed when type checking enabled): <https://github.com/fsprojects/FSharpLint/issues/113>
- Partially fixed bug (fixed when type checking enabled): <https://github.com/fsprojects/FSharpLint/issues/109>
- Configuration can now be written back to XML.
- Configuration API updated to provide management of configuration files.

## [0.2.6] - 2015-07-18
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/108>
- `Default` property now static: <https://github.com/fsprojects/FSharpLint/pull/107>

## [0.2.5] - 2015-07-07
- FSharp.Core.dll is now included again

## [0.2.4] - 2015-07-06
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/101>
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/100>
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/99>
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/103>

## [0.2.3] - 2015-07-02
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/98>

## [0.2.2] - 2015-06-09
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/97>
- XmlDoc rules now configurable to apply to code with certain access levels e.g. private or public. Thanks goes to Jon Hamm for implementing this feature

## [0.2.1] - 2015-05-31
- Included FSharp.Core
- Fixed bug where hints would accidentally match named parameters and property initialisers

## [0.2.0] - 2015-05-31
- Configuration has been updated to be simpler and verifiable via an XSD.
- Type checking is now optional and off by default to speed up the linting.
- More XML documentation rules have been added thanks to [jhamm](https://github.com/jhamm)
- Files can now be ignored by specifying git ignore like globs in the configuration file.
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/78>
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/90>

## [0.1.16] - 2015-03-11
- Handling of project files is now performed by FSharp.Compiler.Service

## [0.1.15] - 2015-02-08
- Added `Enabled` config option to all analysers.

## [0.1.14] - 2015-01-18
- Added a new rule `CanBeReplacedWithComposition` to the `FSharpLint.FunctionReimplementation` analyser: <http://fsprojects.github.io/FSharpLint/FSharpLint.FunctionReimplementation.html>
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/63>

## [0.1.13] - 2015-01-11
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/63>
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/57>
- Files can now be ignored via the .fsproj file: <https://github.com/fsprojects/FSharpLint/issues/55>
- FAKE task now reports on how many files were linted and how many warnings were found.
- FAKE task now includes more detailed information on failure.
- FAKE task now includes an option to fail the build if any warnings are found.

## [0.1.12] - 2014-12-16
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/57>

## [0.1.11] - 2014-11-23
- FSharp.Compiler.Service now included as a strongly named assembly.
- TargetFrameworkVersion now taken from the project file: <https://github.com/fsprojects/FSharpLint/issues/52>

## [0.1.10] - 2014-11-19
- Dropped MSBuild tools back down from 12 to 4 to support VS2010

## [0.1.9] - 2014-11-17
- Updated `FSharp.Compiler.Service` for compatibility with VisualFSharpPowerTools: <https://github.com/fsprojects/FSharpLint/issues/51>

## [0.1.8] - 2014-11-15
- FSharp.Core lookup now supports F# 4, the package is now a single click install in VS 2015 preview

## [0.1.7] - 2014-11-14
- Added support for SuppressMessageAttribute for the Typography analyser
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/48>
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/47>
- Attempted to fix assembly resolution issues by including FParsec built against FSharp.Core 4.3.0.0
- Added FAKE task to the nuget package

## [0.1.6] - 2014-10-25
- Added `FSharpLint.Binding.TupleOfWildcards` rule
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/38>

## [0.1.5] - 2014-10-24
- Added support for `SuppressMessageAttribute` for all analysers except `FSharpLint.Typography`
- Added FAKE task, thanks to [@archaeron](https://github.com/archaeron) for this contribution

## [0.1.4] - 2014-10-21
- Fixed bug: <https://github.com/fsprojects/FSharpLint/issues/36>

## [0.1.3] - 2014-10-16
- Implemented new analyser: `FSharpLint.RaiseWithTooManyArguments`
- Implemented new rule: `FSharpLint.Binding.WildcardNamedWithAsPattern`

## [0.1.2] - 2014-10-14
- More hints and support added for matching if statements, tuples, lists, array, and patterns with hints

## [0.1.1] - 2014-09-28
- Fixed bug: literals in pattern matches were generating lint warnings saying that they should be camel case, whereas they must be pascal case.

## [0.1.0] - 2014-09-25
- First release
