# Formatting

Rules that check F# code formatting rules.

## Sub-groups
* [Tuple Formatting](TupleFormatting.html)
* [Pattern Match Formatting](PatternMatchFormatting.html)
* [Spacing](Spacing.html)

### Rules

#### Typed Item Spacing - FS0010

##### Cause

Checks spacing around a typed item, e.g. (number:int).

##### Rationale

Consistency and readability.

##### How To Fix

Update typed item to use configured spacing.

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)

`typedItemStyle` - style of type item spacing (`NoSpaces`, `SpaceAfter`, `SpacesAround`)

#### Union Definition Indentation - FS0012

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
