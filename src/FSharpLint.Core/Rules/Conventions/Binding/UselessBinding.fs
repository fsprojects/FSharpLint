module FSharpLint.Rules.UselessBinding

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private checkForUselessBinding (checkInfo:FSharpCheckFileResults option) pattern expr range maybeSuggestedFix =
    match checkInfo with
    | Some checkInfo ->
        let rec findBindingIdentifier = function
            | SynPat.Paren(pattern, _) -> findBindingIdentifier pattern
            | SynPat.Named(_, ident, _, _, _) -> Some(ident)
            | _ -> None
        
        let isNotMutable (symbol:FSharpSymbolUse) =
            match symbol.Symbol with
            | :? FSharpMemberOrFunctionOrValue as v -> not v.IsMutable
            | _ -> true

        let checkNotMutable (ident:Ident) = fun () ->
            let symbol =
                checkInfo.GetSymbolUseAtLocation(
                    ident.idRange.StartLine, ident.idRange.EndColumn, "", [ident.idText])

            match symbol with
            | Some(symbol) -> isNotMutable symbol
            | None -> false

        let rec matchingIdentifier (bindingIdent:Ident) = function
            | SynExpr.Paren(expr, _, _, _) ->
                matchingIdentifier bindingIdent expr
            | SynExpr.Ident(ident) when ident.idText = bindingIdent.idText -> Some ident
            | _ -> None

        findBindingIdentifier pattern
        |> Option.bind (fun bindingIdent -> matchingIdentifier bindingIdent expr)
        |> Option.map (fun ident ->
            { Range = range
              Message = Resources.GetString("RulesUselessBindingError")
              SuggestedFix = Some (lazy(maybeSuggestedFix))
              TypeChecks = [ checkNotMutable ident ] })
        |> Option.toArray
    | _ -> Array.empty

let private runner (args:AstNodeRuleParams) =
    let maybeSuggestedFix = 
        match args.GetParents(args.NodeIndex) with
        | AstNode.ModuleDeclaration(SynModuleDecl.Let(_, _, range)) :: _ ->
            Some({ FromRange = range; FromText = "let"; ToText = "" })
        | AstNode.Expression(SynExpr.LetOrUse(_, false, _, _, range)) :: _ -> 
            Some({ FromRange = range; FromText = "use"; ToText = "" })
        | _ -> None
    match args.AstNode with
    | AstNode.Binding(SynBinding(_, _, _, isMutable, _, _, _, pattern, _, expr, range, _))
        when maybeSuggestedFix.IsSome && not isMutable ->
            checkForUselessBinding args.CheckInfo pattern expr range maybeSuggestedFix
    | _ ->
        Array.empty

let rule =
    { Name = "UselessBinding"
      Identifier = Identifiers.UselessBinding
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule

