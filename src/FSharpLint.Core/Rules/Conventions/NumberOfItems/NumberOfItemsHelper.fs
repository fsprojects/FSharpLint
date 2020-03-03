module FSharpLint.Rules.Helper.NumberOfItems

[<RequireQualifiedAccess>]
type Config = {
    // fsharplint:disable RecordFieldNames
    maxItems : int
    // fsharplint:enable RecordFieldNames
}