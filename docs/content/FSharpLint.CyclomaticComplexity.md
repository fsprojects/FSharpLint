#CyclomaticComplexity

Single rule that checks the [cyclomatic complexity](http://en.wikipedia.org/wiki/Cyclomatic_complexity) of functions.

#####Cause

A function has more than a configurable number (`MaxCyclomaticComplexity`) of paths of flow control.

#####Rationale

A high cyclomatic complexity indicates that a function is difficult to understand.

#####How To Fix

Extract some paths of code from the function into other functions.

###Analyser Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)

`MaxCyclomaticComplexity` - An integer property that specifies the max complexity of a function before issuing an error. (Default 10)

`IncludeMatchStatements` - A boolean property that decides whether or not match statements are to be included as different flow paths, the reason why we have this is it's very easy to end up with a high cyclomatic complexity when matching against a large discriminated union. (Default false)