namespace FSharpLint.Core

[<AutoOpen>]
module Prelude =

    module Async =
        let combine operation firstAsync secondAsync = async {
            let! firstOperation = firstAsync 
            let! secondOperation = secondAsync 
            return operation firstOperation secondOperation }

        let map operation xAsync = async {
            let! xAsyncArg = xAsync
            return operation xAsyncArg }