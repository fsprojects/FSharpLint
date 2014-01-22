namespace MattMcveigh.FSharpLint

open Microsoft.FSharp.Compiler.SourceCodeServices

module Program =

    /// Tokenize a single line of F# code
    let rec tokenizeLine (tokenizer:LineTokenizer) state list =
        match tokenizer.ScanToken(state) with
        | Some tok, state ->
            tokenizeLine tokenizer state (tok :: list)
        | None, state -> list, state

    [<EntryPoint>]
    let main argv = 

        let sourceTok = SourceTokenizer([], "C:\\test.fsx")

        let tokenizer = sourceTok.CreateLineTokenizer("let answer = 45")

        let (tokens, _) = tokenizeLine tokenizer 0L []

        //whitespaceAroundOperators tokens (fun str -> System.Console.Write(str + " "))

        tokens |> List.rev |> List.iter (fun tok -> System.Console.Write(tok.TokenName + " "))

        System.Console.ReadKey() |> ignore

        0