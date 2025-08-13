namespace FSharpLint.Core

[<AutoOpen>]
module Prelude =

    module Async =
        let combine operation job1 job2 = async {
            let! firstOperation = job1
            let! secondOperation = job2
            return operation firstOperation secondOperation }

        let map operation job = async {
            let! jobResult = job
            return operation jobResult }
