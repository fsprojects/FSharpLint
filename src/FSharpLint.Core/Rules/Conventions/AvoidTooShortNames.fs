module FSharpLint.Rules.AvoidTooShortNames

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

let private checkIdentifierPart (identifier:Ident) (idText:string) =
    let formatError errorName =
        String.Format(Resources.GetString errorName, idText)

    "RulesAvoidTooShortNamesError" |> formatError |> Array.singleton
    
let private checkIdentifier (identifier:Ident) (idText:string) =
    if idText.Length = 1 && not (idText.StartsWith '_') then
        checkIdentifierPart identifier idText
        |> Array.map (fun message ->
            { Range = identifier.idRange
              Message = message
              SuggestedFix = None
              TypeChecks = List.Empty })
    else
        Array.empty

let private getParameterWithBelowMinimumLength (pats: SynPat list): (Ident * string * (unit -> bool) option) array =
    let rec loop patArray acc =
        match patArray with
        | SynPat.Named(_, ident, _, _, _)::tail ->
            if ident.idText.Length = 1 then
                Array.singleton (ident, ident.idText, None) |> Array.append acc |> loop tail 
            else
                loop tail acc
        | _ -> acc
    loop pats Array.empty

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, _, pattern, _, _, _, _)) ->
        match pattern with
        | SynPat.LongIdent(LongIdentWithDots(identifiers, _),_, _, SynArgPats.Pats(names), _, _) ->
            match identifiers with
            | head::_  ->
                let result: (Ident * string * (unit -> bool) option) array = getParameterWithBelowMinimumLength names
                if head.idText.Length = 1 then
                    Array.append result (Array.singleton (head, head.idText, None))  
                else 
                    result
            | _ -> Array.empty
        | SynPat.Named(_, identifier, _, _, _) when identifier.idText.Length = 1 ->
            (identifier, identifier.idText, None) |> Array.singleton
        | _ -> Array.empty
    | _ -> Array.empty

let runner (args:AstNodeRuleParams) =
    getIdentifiers args
    |> Array.collect (fun (identifier, idText, typeCheck) ->
        let suggestions = checkIdentifier identifier idText
        suggestions |> Array.map (fun suggestion -> { suggestion with TypeChecks = Option.toList typeCheck }))

let rule =
    { Name = "AvoidTooShortNames"
      Identifier = Identifiers.AvoidTooShortNames
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
