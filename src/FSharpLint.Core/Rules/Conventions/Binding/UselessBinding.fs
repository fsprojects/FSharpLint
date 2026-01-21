module FSharpLint.Rules.UselessBinding

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

[<TailCall>]
let rec private matchingIdentifier (bindingIdent:Ident) = function
    | SynExpr.Paren(expr, _, _, _) ->
        matchingIdentifier bindingIdent expr
    | SynExpr.Ident(ident) when ident.idText = bindingIdent.idText -> Some ident
    | _ -> None

let private runner (args:AstNodeRuleParams) =
    let checkForUselessBinding (checkInfo:FSharpCheckFileResults option) pattern expression range maybeSuggestedFix =
        match checkInfo with
        | Some checkFileResults ->
            let rec findBindingIdentifier = function
                | SynPat.Paren(pat, _) -> findBindingIdentifier pat
                | SynPat.Named(SynIdent(ident, _), _, _, _) -> Some(ident)
                | _ -> None
        
            let isNotMutable (symbol:FSharpSymbolUse) =
                match symbol.Symbol with
                | :? FSharpMemberOrFunctionOrValue as fsharpElement -> not fsharpElement.IsMutable
                | _ -> true

            let checkNotMutable (ident:Ident) = fun () ->
                let maybeSymbol =
                    checkFileResults.GetSymbolUseAtLocation(
                        ident.idRange.StartLine, ident.idRange.EndColumn, String.Empty, [ident.idText])

                match maybeSymbol with
                | Some(symbol) -> isNotMutable symbol
                | None -> false

            findBindingIdentifier pattern
            |> Option.bind (fun bindingIdent -> matchingIdentifier bindingIdent expression)
            |> Option.map (fun ident ->
                { Range = range
                  Message = Resources.GetString("RulesUselessBindingError")
                  SuggestedFix = Some (lazy(maybeSuggestedFix))
                  TypeChecks = [ checkNotMutable ident ] })
            |> Option.toArray
        | _ -> Array.empty

    let maybeSuggestedFix = 
        match args.GetParents(args.NodeIndex) with
        | AstNode.ModuleDeclaration(SynModuleDecl.Let(_, _, range)) :: _ ->
            Some({ FromRange = range; FromText = "let"; ToText = String.Empty })
        | AstNode.Expression(SynExpr.LetOrUse(_, false, _, _, range, _)) :: _ -> 
            Some({ FromRange = range; FromText = "use"; ToText = String.Empty })
        | _ -> None
    match args.AstNode with
    | AstNode.Binding(SynBinding(_, _, _, isMutable, _, _, _, pattern, _, expr, range, _, _))
            when maybeSuggestedFix.IsSome && not isMutable ->
                checkForUselessBinding args.CheckInfo pattern expr range maybeSuggestedFix
    | _ ->
        Array.empty

let rule =
    AstNodeRule
        {
            Name = "UselessBinding"
            Identifier = Identifiers.UselessBinding
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
