# Binding

Set of rules that analyse possible mistakes when binding values.

### Analyser Settings

`Enabled` - A boolean property that can enable and disable this analyser. (Default true)

### Rules

#### FavourIgnoreOverLetWild

##### Cause

A value is binded to a wildcard e.g. `let _ = Console.ReadLine()`

##### Rationale

Using the ignore function makes it clear what is intended to happen, rather than something that may be a mistake.

##### How To Fix

Pipe the value into the ignore function e.g. `Console.ReadLine() |> ignore`

##### Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)

#### UselessBinding

##### Cause

An identifier is binded to itself e.g. `let x = x`

##### Rationale

Pointless statement likely to be an error.

##### How To Fix

Remove the binding.

##### Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)

#### WildcardNamedWithAsPattern

##### Cause

A wildcard is given a name using the as pattern e.g. `match something with | _ as x -> x + y`

##### Rationale

The wildcard and as pattern can be replaced with the identifier the value is to be bound to.

##### How To Fix

Replace the wildcard with the identifier the wildcard is currently being bound to, e.g. change `match something with | _ as x -> x + y` to `match something with | x -> x + y`

##### Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)

#### TupleOfWildcards

##### Cause

A constructor in a pattern has arguments that consist entirely of wildcards e.g. `SynPat.Paren(_, _)`

##### Rationale

The tuple of wildcards can be replaced with a single wildcard.

##### How To Fix

Replace the tuple with a single wildcard e.g. the example in the cause could be turned into `SynPat.Paren(_)`

##### Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)