# NumberOfItems

Set of rules that analyse the number of items in a segment of code, for example the number of members in a class.

### Analyser Settings

`Enabled` - A boolean property that can enable and disable this analyser. (Default true)

### Rules

#### MaxNumberOfFunctionParameters

##### Cause

A function contains more than a configurable number of parameters, for example if `MaxItems` was set to 5 (the default value)
then the following condition would cause the error: `let findCat one two three four five six = 0`

##### Rationale

Too many parameters make the function difficult to use.

##### How To Fix

Reduce the number of function parameters, e.g. to fix the example in the "Cause": `let findCat one two three four five = 0`. A good way to reduce the number of parameters is to group them using records.

##### Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)
`MaxItems` - An integer property that specifies the max number of function parameters. (Default 5)

#### MaxNumberOfMembers

##### Cause

A class contains more than a configurable number of members (`MaxItems`).

##### Rationale

The class is likely to be doing too much and violating the single responsibility principle.

##### How To Fix

Reduce the number of members in the class, e.g. extract them out to another class.

##### Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)
`MaxItems` - An integer property that specifies the max number of members in a class. (Default 32)

#### MaxNumberOfItemsInTuple

##### Cause

A tuple contains more than a configurable number of items, for example if `MaxItems` was set to 4 (the default value)
then the following statement would cause the error: `let tup = (1, 2, 3, 5, 6)`

##### Rationale

Tuple's items are not named, the more items there are the harder it is to work out what each is for.

##### How To Fix

Reduce the number of items in the tuple, ideally replace the tuple with a record.

##### Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)
`MaxItems` - An integer property that specifies the max number of items in a tuple. (Default 4)

#### MaxNumberOfBooleanOperatorsInCondition

##### Cause

A `while/if/assert/match when` condition contains more than a configurable number of boolean operators, for example if `MaxItems` was set to 4 (the default value)
then the following condition would cause the error: `if x && y || q || r && t && w then`

##### Rationale

Can make the control flow become diffcult to understand.

##### How To Fix

Reduce the number of boolean operators in the `while/if/assert/match when` condition, e.g. a simple way to fix the example in the "Cause" section you could name the expression:

    let catIsInBin = x && y || q || r && t && w
    if catIsInBin then

##### Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)
`MaxItems` - An integer property that specifies the max number of boolean operators in a condition. (Default 4)