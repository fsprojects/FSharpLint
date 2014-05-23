#FSharpLint

###About

FSharpLint is a style checking tool for F#. It points out locations where a set of rules on how F# is to be styled have been broken.
The tool is configurable via XML and can be run from a console app, or inside Visual Studio using the Visual Studio extension.

While the tool runnable right now, it has a limited set of rules and is probably not stable or well supported across systems - it's very early in development.

The project aims to let the user know of problems through [matching user defined hints](https://github.com/duckmatt/FSharpLint/wiki/Hints) a la [HLint](http://community.haskell.org/~ndm/hlint/), and also by using custom rules written in F# similar to the rules in [Mascot](http://mascot.x9c.fr/manual.html) and [StyleCop](http://stylecop.codeplex.com/).

###Documentation

Documentation is a work in progress at the moment, each project directory will contain a 
README.md file with a brief technical overview of the code. The [wiki](https://github.com/duckmatt/FSharpLint/wiki) 
will contain an overview of the tool and how to use it.

###Support

####Lint

* Windows 7 and 8
* FSharp 3.0

####Visual Studio Extension

* Windows 7 and 8
* FSharp 3.0
* Visual Studio 2012

###Dependencies

* NUnit
* FSharp.Data (a built dll is included, reason is that I want to be able to build this project on Windows 7)
* FSharp.Compiler.Services

###Licensing

The project is licensed under GPLv3. For more information on the license see the LICENSE file and http://www.gnu.org/licenses/quick-guide-gplv3.html.

###Contact

Feel free to post an issue on [github](https://github.com/duckmatt/FSharpLint/issues) if you have any questions, have suggestions, or have found a defect.
Aslo I can be contacted via email at [p4rk0ur@hotmail.co.uk](mailto:p4rk0ur@hotmail.co.uk)
