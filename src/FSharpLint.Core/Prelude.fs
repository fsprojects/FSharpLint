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

    module List =

        /// Partitions a list of Results into two lists where the first contains all
        /// Oks and the second contains all Errors.
        let partitionChoices (xs:Result<_, _> list) =
            let (oks, errs) =
                xs
                |> List.fold (fun (oks, errs) x ->
                    match x with
                    | Ok ok -> (ok :: oks, errs)
                    | Error err -> (oks, err :: errs)) ([], [])

            (List.rev oks, List.rev errs)