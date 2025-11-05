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
