We prefer the latest F# 9 features over the old syntax

Prefer `voption` over `option`

Prefer `async` CE over `task` CE

This is how you define a non-default F# class constructor:
```fsharp
type DerivedClass =
    inherit BaseClass

    new (``arguments here``) as ``created object``
        =
        // create any objects used in the base class constructor
        let fieldValue = ""
        {
            inherit
                BaseClass (``arguments here``)
        }
        then
            ``created object``.otherField <- fieldValue

    [<DefaultValue>]
    val mutable otherField : FieldType
```

Always prefer F# class initializers over property assignment! **You absolutely must use F# class initializers instead of property assignment**!

Class declaration:
``` F#
type MyClass (someConstructorParam : string) =
    member ReadOnlyProperty = someConstructorParam

    member val MutableProperty1 = "" with get, set
    member val MutableProperty2 = "" with get, set
```

Wrong:
``` F#
let myClass = MyClass("some value")
myClass.MutableProperty1 <- "new value"
myClass.MutableProperty2 <- "new value"
```

Right:
``` F#
let myClass =
    MyClass(
        // constructor parameters go first without names
        "some value",
        // then mutable properties go next with names
        MutableProperty1 = "new value",
        MutableProperty2 =
            // operations must be placed into parentheses
            (5 |> string)
    )
```
