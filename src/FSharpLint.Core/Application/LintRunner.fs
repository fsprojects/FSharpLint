module FSharpLint.Application.LintRunner

open System
open FSharp.Compiler
open FSharpLint.Core
open FSharpLint.Framework
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.Rules
open FSharpLint.Rules

type internal Context =
    { IndentationRuleContext : Map<int,bool*int>
      NoTabCharactersRuleContext : (string * Range.range) list }

let internal runAstNodeRules (rules:RuleMetadata<AstNodeRuleConfig> []) (globalConfig:Rules.GlobalRuleConfig) typeCheckResults (filePath:string) (fileContent:string) (lines:string []) syntaxArray skipArray =
    let mutable indentationRuleState = Map.empty
    let mutable noTabCharactersRuleState = List.empty

    // Collect suggestions for AstNode rules, and build context for following rules.
    let astNodeSuggestions =
        syntaxArray
        |> Array.mapi (fun i astNode -> (i, astNode))
        |> Array.collect (fun (i, astNode) ->
            let getParents (depth:int) = AbstractSyntaxArray.getBreadcrumbs depth syntaxArray skipArray i
            let astNodeParams =
                { AstNode = astNode.Actual
                  NodeHashcode = astNode.Hashcode
                  NodeIndex =  i
                  SyntaxArray = syntaxArray
                  SkipArray = skipArray
                  GetParents = getParents
                  FilePath = filePath
                  FileContent = fileContent
                  Lines = lines
                  CheckInfo = typeCheckResults
                  GlobalConfig = globalConfig }
            // Build state for rules with context.
            indentationRuleState <- Indentation.ContextBuilder.builder indentationRuleState astNode.Actual
            noTabCharactersRuleState <- NoTabCharacters.ContextBuilder.builder noTabCharactersRuleState astNode.Actual

            rules
            |> Array.collect (fun rule -> runAstNodeRule rule astNodeParams))

    let context =
        { IndentationRuleContext = indentationRuleState
          NoTabCharactersRuleContext = noTabCharactersRuleState }

    rules |> Array.iter (fun rule -> rule.RuleConfig.Cleanup())
    (astNodeSuggestions, context)

let internal runLineRules (lineRules:Configuration.LineRules) (globalConfig:Rules.GlobalRuleConfig) (filePath:string) (fileContent:string) (lines:string []) (context:Context) =
    fileContent
    |> String.toLines
    |> Array.collect (fun (line, lineNumber, isLastLine) ->
        let lineParams =
            { LineRuleParams.Line = line
              LineNumber = lineNumber + 1
              IsLastLine = isLastLine
              FilePath = filePath
              FileContent = fileContent
              Lines = lines
              GlobalConfig =  globalConfig }

        let indentationError =
            lineRules.IndentationRule
            |> Option.map (fun rule -> runLineRuleWithContext rule context.IndentationRuleContext lineParams)

        let noTabCharactersError =
            lineRules.NoTabCharactersRule
            |> Option.map (fun rule -> runLineRuleWithContext rule context.NoTabCharactersRuleContext lineParams)

        let lineErrors =
            lineRules.GenericLineRules
            |> Array.collect (fun rule -> runLineRule rule lineParams)

        [|
            indentationError |> Option.toArray
            noTabCharactersError |> Option.toArray
            lineErrors |> Array.singleton
        |])
    |> Array.concat
    |> Array.concat
