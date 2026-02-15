module FSharpLint.Core.Tests.TestHintMatcherBase

open System
open FParsec
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Application
open FSharpLint.Framework
open FSharpLint.Framework.HintParser
open FSharpLint.Framework.MergeSyntaxTrees
open FSharpLint.Framework.ParseFile
open FSharpLint.Rules

open FSharpLint.Framework.Rules
open FSharpLint.Rules.HintMatcher

[<AbstractClass>]
type TestHintMatcherBase () =
    inherit TestRuleBase.TestRuleBase()

    let mutable hintTrie = Edges.Empty

    let generateHintConfig hints =
        let parseHints hintStrings =
            let parseHint hintString =
                match CharParsers.run phint hintString with
                | FParsec.CharParsers.Success(hint, _, _) -> hint
                | FParsec.CharParsers.Failure(error, _, _) -> failwithf "Invalid hint %s" error

            List.map parseHint hintStrings

        parseHints hints
        |> MergeSyntaxTrees.mergeHints

    member this.SetConfig (hints:string list) =
        hintTrie <- generateHintConfig hints

    override this.Parse (input:string, ?maybeFileName:string, ?checkFile:bool, ?globalConfig:GlobalRuleConfig) =
        let checker = FSharpChecker.Create(keepAssemblyContents=true)

        let parseResults =
            match maybeFileName with
            | Some fileName ->
                ParseFile.parseSourceFile fileName input checker
            | None ->
                ParseFile.parseSource input checker

        let astNodeRule =
            match HintMatcher.rule { HintTrie = hintTrie } with
            | Rules.AstNodeRule nodeRule -> nodeRule
            | _ -> failwith "TestHintMatcherBase only accepts AstNodeRules"

        let resolvedGlobalConfig = Option.defaultValue GlobalRuleConfig.Default globalConfig

        match Async.RunSynchronously parseResults with
        | ParseFileResult.Success parseInfo ->
            let syntaxArray = AbstractSyntaxArray.astToArray parseInfo.Ast
            let checkResult =
                match checkFile with
                | Some false -> None
                | _ -> parseInfo.TypeCheckResults
            let suggestions =
                runAstNodeRules
                    {
                        Rules = Array.singleton astNodeRule
                        GlobalConfig = resolvedGlobalConfig
                        TypeCheckResults = checkResult
                        ProjectCheckResults = None
                        ProjectOptions = Lazy<_>()
                        FilePath = (Option.defaultValue String.Empty maybeFileName)
                        FileContent = input
                        Lines = (input.Split("\n"))
                        SyntaxArray = syntaxArray
                    }
                |> fst
            Array.iter this.PostSuggestion suggestions
        | _ ->
            failwith "Failed to parse"
