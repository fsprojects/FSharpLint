#FunctionReimplementation

Set of rules that highlight lambda functions that can removed through eta reduction.

###Analyser Settings

`Enabled` - A boolean property that can enable and disable this analyser. (Default true)

###Rules

####ReimplementsFunction

#####Cause

A lambda function does nothing other than call an existing function, two examples below:

`fun x y -> x + y`
`fun x y -> foo x y`

#####Rationale

The lambda functions are redundant.

#####How To Fix

Replace the lambda with the function that is being called.

`fun x y -> x + y` is the same as `(+)`
`fun x y -> foo x y` is the same as `foo`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)

####CanBeReplacedWithComposition

#####Cause

A lambda function applies a single argument to a chain of function calls, two examples below:

`fun x -> not(isValid(x))`
`fun x -> x |> isValid |> not` can be refactored to 

#####Rationale

The lambda functions are redundant.

#####How To Fix

Replace the lambda with function composition:

`fun x -> not(isValid(x))` and `fun x -> x |> isValid |> not` are the same as `isValid >> not`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)