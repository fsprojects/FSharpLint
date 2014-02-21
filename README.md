# FSharpLint

Attempt at a lint tool for F#.

## Rules somewhat covered right now:

* Naming conventions following FSharp guidelines, further improvements to be made: any value with the Literal attribute should be pascal case, any public value/function should be camel case or pascal case (right now restricted to camel case), and the spelling of words should be checked against a dictionary.
* Suggests to use the ignore function over 'let _ = expr'
* Restricts the number of parameters a function can have (right now it's set as 5 params, but this should be configurable).

At the moment there is no configuration, however when it is introduced I'm planning on using the same format as StyleCop.

## Project Layout

* src/FSharpLint.Console is a console app project that at the moment runs the rules against a literal string as a fs script file and displays any errors on the console.
* src/FSharpLint.Rules is where each rule is implemented inside of its own module.
* src/FSharpLint.Framework contains common functions and types.

## Some documents to go off of:

* FSharp guidelines: http://research.microsoft.com/en-us/um/cambridge/projects/fsharp/manual/fsharp-component-design-guidelines.pdf
* OCaml guidelines: http://caml.inria.fr/resources/doc/guides/guidelines.en.html
* Mascot (an OCaml lint) documentation: https://forge.ocamlcore.org/frs/download.php/988/mascot.pdf