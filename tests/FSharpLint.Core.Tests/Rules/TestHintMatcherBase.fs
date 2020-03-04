module TestHintMatcherBase

open FParsec
open FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.HintParser
open FSharpLint.Framework.HintParser.MergeSyntaxTrees
open FSharpLint.Framework.ParseFile
open FSharpLint.Rules

open FSharpLint.Framework.Rules
open FSharpLint.Rules.HintMatcher

let private generateHintConfig hints =
    let parseHints hints =
        let parseHint hint =
            match CharParsers.run phint hint with
            | FParsec.CharParsers.Success(hint, _, _) -> hint
            | FParsec.CharParsers.Failure(error, _, _) -> failwithf "Invalid hint %s" error

        List.map parseHint hints

    parseHints hints
    |> MergeSyntaxTrees.mergeHints

[<AbstractClass>]
type TestHintMatcherBase () =
    inherit TestRuleBase.TestRuleBase()

    let mutable hintTrie = Edges.Empty

    member this.SetConfig (hints:string list) =
        hintTrie <- generateHintConfig hints

    override this.Parse (input:string, ?fileName:string, ?checkFile:bool, ?globalConfig:GlobalRuleConfig) =
        let checker = FSharpChecker.Create()

        let parseResults =
            match fileName with
            | Some fileName ->
                ParseFile.parseSourceFile fileName input checker
            | None ->
                ParseFile.parseSource input checker

        let rule =
            match HintMatcher.rule { HintTrie = hintTrie }with
            | Rules.AstNodeRule rule -> rule
            | _ -> failwithf "TestHintMatcherBase only accepts AstNodeRules"

        let globalConfig = globalConfig |> Option.defaultValue GlobalRuleConfig.Default

        match parseResults with
        | ParseFileResult.Success parseInfo ->
            let (syntaxArray, skipArray) = AbstractSyntaxArray.astToArray parseInfo.Ast
            let checkResult =
                match checkFile with
                | Some false -> None
                | _ -> parseInfo.TypeCheckResults
            let suggestions = runAstNodeRules (Array.singleton rule) globalConfig checkResult (Option.defaultValue "" fileName) input syntaxArray skipArray |> fst
            suggestions |> Array.iter this.PostSuggestion
        | _ ->
            failwithf "Failed to parse"