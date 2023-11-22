---
title: Rule Config
category: how-to
menu_order: 3
---

# Rule Configuration

## Creating a Configuration File

The linter by default looks for a file named `fsharplint.json` in the current working directory. Typically you would have this file in the root of your project.

At this moment in time the configuration requires every rule to be added to your file, rather than a typical approach where you would override just the rules you want to change from their defaults. This will be addressed in a future version see: [Issue #488](https://github.com/fsprojects/FSharpLint/issues/488).

Check out the [default configuration](https://github.com/fsprojects/FSharpLint/blob/master/src/FSharpLint.Core/fsharplint.json) that the tool comes with to see all possible settings and their default values.

## Global Configuration

In addition to the configuration available for each rule, there are some settings which are defined globally to maintain consistency across
multiple rules. These are defind in the `globals` object in the base of the configuration file. The rule pages below point out any global
setting that each rule depends on. The config is as follows, but can be completely omitted to use defaults:

    {
      "globals": {
        "numIndentationSpaces": 4 // number of spaces used for indentation
      }
    }

## Ignoring Files

In the configuration file paths can be used to specify files that should be included, globs are used to match wildcard directories and files. For example the following will match all files with the file name assemblyinfo (the matching is case insensitive) with any extension:

    { "ignoreFiles": ["assemblyinfo.*"] }

* Directories in the path must be separated using `/`
* If the path ends with a `/` then everything inside of a matching directory shall be excluded.
* If the path does not end with a `/` then all matching files are excluded.


## <a name="ruleList"></a>Rule List

The following rules can be specified for linting.

- [TupleCommaSpacing (FL0001)](rules/FL0001.html)
- [TupleIndentation (FL0002)](rules/FL0002.html)
- [TupleParentheses (FL0003)](rules/FL0003.html)
- [PatternMatchClausesOnNewLine (FL0004)](rules/FL0004.html)
- [PatternMatchOrClausesOnNewLine (FL0005)](rules/FL0005.html)
- [PatternMatchClauseIndentation (FL0006)](rules/FL0006.html)
- [PatternMatchExpressionIndentation (FL0007)](rules/FL0007.html)
- [ModuleDeclSpacing (FL0008)](rules/FL0008.html)
- [ClassMemberSpacing (FL0009)](rules/FL0009.html)
- [TypedItemSpacing (FL0010)](rules/FL0010.html)
- [TypePrefixing (FL0011)](rules/FL0011.html)
- [UnionDefinitionIndentation (FL0012)](rules/FL0012.html)
- [RecursiveAsyncFunction (FL0013)](rules/FL0013.html)
- [RedundantNewKeyword (FL0014)](rules/FL0014.html)
- [NestedStatements (FL0015)](rules/FL0015.html)
- [FailwithWithSingleArgument (FL0016)](rules/FL0016.html)
- [RaiseWithSingleArgument (FL0017)](rules/FL0017.html)
- [NullArgWithSingleArgument (FL0018)](rules/FL0018.html)
- [InvalidOpWithSingleArgument (FL0019)](rules/FL0019.html)
- [InvalidArgWithTwoArguments (FL0020)](rules/FL0020.html)
- [FailwithfWithArgumentsMatchingFormattingString (FL0021)](rules/FL0021.html)
- [MaxLinesInLambdaFunction (FL0022)](rules/FL0022.html)
- [MaxLinesInMatchLambdaFunction (FL0023)](rules/FL0023.html)
- [MaxLinesInValue (FL0024)](rules/FL0024.html)
- [MaxLinesInFunction (FL0025)](rules/FL0025.html)
- [MaxLinesInMember (FL0026)](rules/FL0026.html)
- [MaxLinesInConstructor (FL0027)](rules/FL0027.html)
- [MaxLinesInProperty (FL0028)](rules/FL0028.html)
- [MaxLinesInModule (FL0029)](rules/FL0029.html)
- [MaxLinesInRecord (FL0030)](rules/FL0030.html)
- [MaxLinesInEnum (FL0031)](rules/FL0031.html)
- [MaxLinesInUnion (FL0032)](rules/FL0032.html)
- [MaxLinesInClass (FL0033)](rules/FL0033.html)
- [ReimplementsFunction (FL0034)](rules/FL0034.html)
- [CanBeReplacedWithComposition (FL0035)](rules/FL0035.html)
- [InterfaceNames (FL0036)](rules/FL0036.html)
- [ExceptionNames (FL0037)](rules/FL0037.html)
- [TypeNames (FL0038)](rules/FL0038.html)
- [RecordFieldNames (FL0039)](rules/FL0039.html)
- [EnumCasesNames (FL0040)](rules/FL0040.html)
- [UnionCasesNames (FL0041)](rules/FL0041.html)
- [ModuleNames (FL0042)](rules/FL0042.html)
- [LiteralNames (FL0043)](rules/FL0043.html)
- [NamespaceNames (FL0044)](rules/FL0044.html)
- [MemberNames (FL0045)](rules/FL0045.html)
- [ParameterNames (FL0046)](rules/FL0046.html)
- [MeasureTypeNames (FL0047)](rules/FL0047.html)
- [ActivePatternNames (FL0048)](rules/FL0048.html)
- [PublicValuesNames (FL0049)](rules/FL0049.html)
- [<s>NonPublicValuesNames (FL0050)</s>](rules/FL0050.html) (Removed in `0.20.0`)
- [MaxNumberOfItemsInTuple (FL0051)](rules/FL0051.html)
- [MaxNumberOfFunctionParameters (FL0052)](rules/FL0052.html)
- [MaxNumberOfMembers (FL0053)](rules/FL0053.html)
- [MaxNumberOfBooleanOperatorsInCondition (FL0054)](rules/FL0054.html)
- [FavourIgnoreOverLetWild (FL0055)](rules/FL0055.html)
- [WildcardNamedAsPattern (FL0056)](rules/FL0056.html)
- [UselessBinding (FL0057)](rules/FL0057.html)
- [TupleOfWildcards (FL0058)](rules/FL0058.html)
- [Indentation (FL0059)](rules/FL0059.html)
- [MaxCharactersOnLine (FL0060)](rules/FL0060.html)
- [TrailingWhitespaceOnLine (FL0061)](rules/FL0061.html)
- [MaxLinesInFile (FL0062)](rules/FL0062.html)
- [TrailingNewLineInFile (FL0063)](rules/FL0063.html)
- [NoTabCharacters (FL0064)](rules/FL0064.html)
- [Hints (FL0065)](rules/FL0065.html)
- [NoPartialFunctions (FL0066)](rules/FL0066.html)
- [PrivateValuesNames (FL0067)](rules/FL0067.html)
- [InternalValuesNames (FL0068)](rules/FL0068.html)
- [GenericTypesNames (FL0069)](rules/FL0069.html)
- [FavourTypedIgnore (FL0070)](rules/FL0070.html)
- [CyclomaticComplexity (FL0071)](rules/FL0071.html)
- [FailwithBadUsage (FL0072)](rules/FL0072.html)
- [FavourReRaise (FL0073)](rules/FL0073.html)
- [FavourConsistentThis (FL0074)](rules/FL0074.html)
- [AvoidTooShortNames (FL0075)](rules/FL0075.html)
- [FavourStaticEmptyFields (FL0076)](rules/FL0076.html)
- [IndexerAccessorStyleConsistency (FL0077)](rules/FL0077.html)
