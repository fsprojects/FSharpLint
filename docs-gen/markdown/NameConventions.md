# Name Conventions

Set of rules that analyse the naming of user defined elements within a program.

## Config

Each of these rules has the following available settings. Any of them can be left empty and they will not be checked.

`enabled` - A boolean property that can enable and disable this rule.

`naming` - the expected case of the element (`PascalCase`, `CamelCase`)

`underscores` - if underscores are allowed in the element (`AllowPrefix`, `AllowAny`, `None`)

`prefix` - expected prefix for this element

`suffix` - expected suffix for this element

### Rules

Each of the following rules targets a different kind of element.

- InterfaceNames - FS0036
- ExceptionNames - FS0037
- TypeNames - FS0038
- RecordfieldNames - FS0039
- EnumCasesNames - FS0040
- UnionCasesNames - FS0041
- ModuleNames - FS0042
- LiteralNames - FS0043
- NamespaceNames - FS0044
- MemberNames - FS0045
- ParameterNames - FS0046
- MeasureTypeNames - FS0047
- ActivePatternNames - FS0048
- PublicValuesNames - FS0049
- NonPublicValuesNames - FS0050

### Example

Check the sample how to ignore the FS0050 warnings:

        let willShowWarning () =
        let WithWarning = None
        [<System.Diagnostics.CodeAnalysis.SuppressMessage("*", "NonPublicValuesNames")>]
        let NoWarning = None
        NoWarning, WithWarning

