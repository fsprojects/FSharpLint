namespace FSharpLint.Core

[<AutoOpen>]
module Prelude =

    module Async =
        let combine f x y = async {
            let! x = x
            let! y = y
            return f x y }

        let map f xAsync = async {
            let! x = xAsync
            return f x }