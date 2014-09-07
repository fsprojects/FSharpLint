#FunctionReimplementation

Single rule that checks a lambda function does not act as an 'alias' for another function.

#####Cause

A lambda function does nothing other than call an existing function, two examples below:

`fun x y -> x + y`
`fun x y -> foo x y`

#####Rationale

The lambda functions are pointless.

#####How To Fix

Replace the lambda with the function that is being called.

`fun x y -> x + y` is the same as `(+)`
`fun x y -> foo x y` is the same as `foo`

###Analyser Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)