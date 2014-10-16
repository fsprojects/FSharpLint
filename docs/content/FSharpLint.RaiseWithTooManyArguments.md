#RaiseWithTooManyArguments

`raise` [can be passed more than one argument](http://visualfsharp.codeplex.com/workitem/41), in code this is likely to be a mistake and these rules warn when this occurs.

###Analyser Settings

`Enabled` - A boolean property that can enable and disable this analyser. (Default true)

###Rules

####RaiseWithSingleArgument

#####Cause

`raise` is passed more than one argument e.g. `raise (System.ArgumentException("Divisor cannot be zero.")) 5`

#####Rationale

`raise` being passed more than one argument (the exception to be thrown) is probably a mistake.

#####How To Fix

Remove the extra arguments.

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)

####FailwithWithSingleArgument

#####Cause

`failwith` is passed more than one argument e.g. `failwith "Divisor cannot be zero." 5`

#####Rationale

`failwith` being passed more than one argument (the error message) is probably a mistake.

#####How To Fix

Remove the extra arguments.

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)

####FailwithfWithArgumentsMatchingFormatString

#####Cause

`failwithf` is passed more arguments than the format string (first argument) species e.g. `failwithf "%d" 5 5`

#####Rationale

`failwithf` being passed more arguments than the format string (first argument) specifies is probably a mistake.

#####How To Fix

Remove the extra arguments.

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)