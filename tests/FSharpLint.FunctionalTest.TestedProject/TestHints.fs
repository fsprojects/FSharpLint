module FSharpLint.FunctionalTest.TestedProject

let main () =
    let meow = not (1 = 1) |> ignore

    let meow = not (1 <> 1)

    let id = fun x -> x

    let dog = not true

    let dog = not false

    let sum = [1;2;3] |> List.fold (+) 0


    let x = true

    if x <> true then
        ()

    ()