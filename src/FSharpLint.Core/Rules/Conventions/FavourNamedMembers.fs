module FSharpLint.Rules.FavourNamedMembers

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open System

let private checkIdentifierPart (identifier:Ident) (idText:string) =
    let formatError errorName =
        String.Format(Resources.GetString errorName, idText)

    "RulesFavourNamedMembersError" |> formatError |> Array.singleton

let private checkIdentifier (identifier:Ident) (idText:string) =
    if idText.Length > 0 then
        checkIdentifierPart identifier idText
        |> Array.map (fun message ->
            { Range = identifier.idRange
              Message = message
              SuggestedFix = None
              TypeChecks = List.Empty })
    else
        Array.empty

let private getUnnamedFields (fields: SynField list): bool =
    let rec loop syncFieldArray =
        match syncFieldArray with
        | SynField(_, _,maybeName, _, _, _, _, _)::tail ->
            match maybeName with
            | Some _ -> 
                loop tail
            | None ->  true
        | _ -> false
    loop fields 

let getIdentifiers args =
    match args.AstNode with
    | AstNode.UnionCase(SynUnionCase(_, identifier, SynUnionCaseKind.Fields fields, _, _, range)) ->
        if getUnnamedFields fields then
            (identifier, identifier.idText, None) |> Array.singleton
        else
            Array.empty
    | AstNode.Match(SynMatchClause(SynPat.LongIdent(LongIdentWithDots (ids, _) ,_ ,_ ,SynArgPats.Pats(_),_,_), _, _, _ ,_)) ->        
        match ids with
        | head::_ -> (head, head.idText, None) |> Array.singleton
        | _ -> Array.empty        
    | _ -> Array.empty

let runner (args:AstNodeRuleParams) =
    getIdentifiers args
    |> Array.collect (fun (identifier, idText, typeCheck) ->
        let suggestions = checkIdentifier identifier idText
        suggestions |> Array.map (fun suggestion -> { suggestion with TypeChecks = Option.toList typeCheck }))

let rule =
    { Name = "FavourNamedMembers"
      Identifier = Identifiers.FavourNamedMembers
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
