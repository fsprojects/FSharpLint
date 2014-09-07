#Binding

Set of rules that analyse possible mistakes when binding values.

###Analyser Settings

`Enabled` - A boolean property that can enable and disable this analyser. (Default true)

###Rules

####FavourIgnoreOverLetWild

#####Cause

A value is binded to a wildcard e.g. `let _ = Console.ReadLine()`

#####Rationale

Using the ignore function makes it clear what is intended to happen, rather than something that may be a mistake.

#####How To Fix

Pipe the value into the ignore function e.g. `Console.ReadLine() |> ignore`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)

####UselessBinding

#####Cause

An identifier is binded to itself e.g. `let x = x`

#####Rationale

Pointless statement likely to be an error.

#####How To Fix

Remove the binding.

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)