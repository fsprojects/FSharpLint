#NameConventions

Set of rules that analyse the naming of user defined elements within a program. The rules and their errors are based on the [F# Component Design Guidelines](http://fsharp.org/specs/component-design-guidelines/)

###Analyser Settings

N/A

###Rules

####IdentifiersMustNotContainUnderscores

#####Cause

Any identifier contains an underscore in its name for example: `let goat_machine = 0`

#####Rationale

*"Historically, some F# libraries have used underscores in names. However, this is no longer widely accepted, partly because it clashes with .NET naming conventions. That said, some F# programmers use underscores heavily, partly for historical reasons, and tolerance and respect is important. However, be aware that the style is often disliked by others who have a choice about whether to use it."* - [F# Component Design Guidelines](http://fsharp.org/specs/component-design-guidelines/)

#####How To Fix

Remove the underscore from the identifier, for the example listed in the cause the fix would be to turn it into: `let goatMachine = 0`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule.

####InterfaceNamesMustBeginWithI

#####Cause

An identifier naming an interface not beginning with the letter `I` e.g. `type Printable = abstract member Print : unit -> unit`

#####Rationale

.NET convention e.g. `IEnumerable`

#####How To Fix

Add `I` onto the beginning of the identifier, for the example listed in the cause, the fix would be to turn it into: `type IPrintable = abstract member Print : unit -> unit`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule.

####ExceptionNamesMustEndWithException

#####Cause

An identifier naming an exception not ending with the word `Exception` e.g. `exception Read of string`

#####Rationale

.NET convention e.g. `NullReferenceException`

#####How To Fix

Add `Exception` onto the end of the identifier, for the example listed in the cause, the fix would be to turn it into: `exception ReadException of string`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule.

####TypeNamesMustBePascalCase

#####Cause

The name of a type was not in PascalCase, for example this would fail: `class foo`

#####Rationale

.NET convention e.g. `LinkedList<T>`

#####How To Fix

Change the name of the type to PascalCase e.g. `class Foo`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule.

####RecordFieldNamesMustBePascalCase

#####Cause

The name of a field in a record was not in PascalCase, for example this would fail: `type Cat = { dog: int }`

#####Rationale

Suggested in [F# Component Design Guidelines](http://fsharp.org/specs/component-design-guidelines/). A record field in C# will be a class property so it makes sense to have it be PascalCase as that's the convention for properties in .NET

#####How To Fix

Change the name of the field to PascalCase e.g. `type Cat = { Dog: int }`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule.

####EnumCasesMustBePascalCase

#####Cause

The name of a case in an enum was not in PascalCase, for example this would fail: `type MyEnum = | enumCase = 1`

#####Rationale

.NET convention e.g. `DayOfWeek.Friday`

#####How To Fix

Change the name of the enum case to PascalCase e.g. `type MyEnum = | EnumCase = 1`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule.

####ModuleNamesMustBePascalCase

#####Cause

The name of a module was not in PascalCase, for example this would fail: `module dog`

#####Rationale

F# convention. Also from C# an F# module will be represented as a static class, and the .NET convention for type names is PascalCase.

#####How To Fix

Change the name of the module to PascalCase e.g. `module Dog`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule.

####LiteralNamesMustBePascalCase

#####Cause

The literal value was not in PascalCase, for example this would fail: `let [<Literal>] foo = 5`

#####Rationale

There's a practical reason here, [you won't be able to pattern match against literals beginning with a lowercase letter](http://msdn.microsoft.com/en-us/library/dd233193.aspx).

#####How To Fix

Change the literal value to PascalCase e.g. `let [<Literal>] Foo = 5`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule.

####NamespaceNamesMustBePascalCase

#####Cause

The namespace was not in PascalCase, for example this would fail: `namespace matt.dog.cat"`

#####Rationale

.NET convention e.g. `System.Collections.Generic`

#####How To Fix

Change the namespace to PascalCase e.g. `namespace Matt.Dog.Cat"`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule.

####MemberNamesMustBePascalCase

#####Cause

The member was not in PascalCase, for example this would fail: `this.member() = ()`

#####Rationale

.NET convention e.g. `this.ToString()`

#####How To Fix

Change the member to PascalCase e.g. `this.Member() = ()`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule.

####ParameterMustBeCamelCase

#####Cause

The parameter in a function or method was not in camelCase, for example this would fail: `let f FooBar = FooBar`

#####Rationale

Non public values should be camelCase, see [NonPublicValuesCamelCase](#NonPublicValuesCamelCase).

#####How To Fix

Change the parameter to camelCase e.g. `let f fooBar = fooBar`

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule.

####NonPublicValuesCamelCase

#####Cause

A function or value that is not externally accessible's identifier is not in camelCase, for example this would fail: 

    let f x = 
        let Woof = 4 * x
        Woof

#####Rationale

*"It is common and accepted F# style to use camelCase for all names bound as local variables or in pattern matches and function definitions. ... It is also common and accepted F# style to use camelCase for locally bound functions"* - [F# Component Design Guidelines](http://fsharp.org/specs/component-design-guidelines/)

Rationale for public values being either PascalCase or CamelCase: 

*"Do use either PascalCase or camelCase for public functions and values in F# modules. camelCase is generally used for public functions which are designed to be used unqualified (e.g. invalidArg), and for the “standard collection functions” (e.g. List.map). In both these cases, the function names act much like keywords in the language."* - [F# Component Design Guidelines](http://fsharp.org/specs/component-design-guidelines/)

#####How To Fix

Change the value's identifier to camelCase e.g.

    let f x = 
        let woof = 4 * x
        woof

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule.
