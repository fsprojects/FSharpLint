# Formatting

Set of rules that check F# code formatting rules

### Rules

#### TupleCommaSpacing

##### Cause

Space missing after tuple comma.

##### Rationale

For readability, it helps to include a space after tuple commas.

##### How To Fix

Add a space after tuple commas.

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)

#### Tuple Indentation

##### Cause

Tuples which span several lines should have consistent indentation.

##### Rationale

Readability.

##### How To Fix

Fix tuple indentation

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)

#### Tuple Parentheses

##### Cause

Missing parentheses around tuple instantiation.

##### Rationale

Recommended by [Microsoft F# code formatting guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-tuples).

##### How To Fix

Add parentheses around tuple instantiation.

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)

#### Pattern Match Clauses On New Line

##### Cause

All pattern match clauses should be on their own line.

##### Rationale

Readability.

##### How To Fix

Place each pattern match clause on its own line.

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)

#### Pattern Match Or Clauses On New Line

##### Cause

All pattern match "or" clauses should be on their own line.

##### Rationale

Readability.

##### How To Fix

Place each pattern match "or" clause on its own line.

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)

#### Pattern Match Clause Indentation

##### Cause

All pattern match clauses should be at the same indentation level.

##### Rationale

Readability.

##### How To Fix

Update pattern match clauses to have consistent indentation.

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)

#### Pattern Match Expression Indentation

##### Cause

All pattern match expressions (to the right of -> after clause) should be at the same indentation level.

##### Rationale

Readability.

##### How To Fix

Update pattern match expressions to have consistent indentation.

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)

#### Module Decl Spacing

##### Cause

Unexpected number of spaces between declarations within module (1 space is expected).

##### Rationale

Recommended by [Microsoft F# code formatting guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-blank-lines).

##### How To Fix

Use 1 space between module declarations.

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)

#### Class Member Spacing

##### Cause

Unexpected number of spaces between declarations within module (2 spaces are expected).

##### Rationale

Recommended by [Microsoft F# code formatting guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-blank-lines).

##### How To Fix

Use 2 space between class members.

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)

#### Typed Item Spacing

##### Cause

Checks spacing around a typed item, e.g. (number:int).

##### Rationale

Consistency and readability.

##### How To Fix

Update typed item to use configured spacing.

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)

`typedItemStyle` - style of type item spacing (`NoSpaces`, `SpaceAfter`, `SpacesAround`)

#### Union Definition Indentation

##### Cause

Incorrect formatting for union definition

##### Rationale

Recommended by [Microsoft F# code formatting guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#use-prefix-syntax-for-generics-foot-in-preference-to-postfix-syntax-t-foo).

##### How To Fix

Update higher order type to use correct format based on linked guide.

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)

#### Type Prefixing

##### Cause

Incorrect formatting for higher order type.

##### Rationale

Recommended by [Microsoft F# code formatting guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-discriminated-union-declarations).

##### How To Fix

Update union definition to have correct formatting as specified in linked guide (indent `|` by 4 spaces).

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)
