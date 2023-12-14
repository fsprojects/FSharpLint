module FSharpLint.Rules.AvoidTooShortNames

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

let private isIdentifierTooShort (identifier: string) =
    identifier.Length < 2 && not (identifier.StartsWith '_')

let private checkIdentifierPart (identifier:Ident) (idText:string) =
    let formatError errorName =
        String.Format(Resources.GetString errorName, idText)

    "RulesAvoidTooShortNamesError" |> formatError |> Array.singleton
    
let private checkIdentifier (identifier:Ident) (idText:string) =
    if isIdentifierTooShort idText then
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
            if isIdentifierTooShort ident.idText then
                Array.singleton (ident, ident.idText, None) |> Array.append acc |> loop tail 
            else
                loop tail acc
        | SynPat.Paren(pat, _)::tail ->
            match pat with
            | SynPat.Typed(typedPat, _, _) ->
                loop (typedPat::tail) acc
            | _ -> loop (pat::tail) acc
        | SynPat.LongIdent(_, _, _, argPats, _, _)::tail ->
            match argPats with
            | SynArgPats.Pats(pats) -> 
                loop (List.append pats tail) acc
            | _ -> 
                loop tail acc
        | _ -> acc
    loop pats Array.empty

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Match(SynMatchClause(namePattern, _, _, _, _)) ->
        getParameterWithBelowMinimumLength [namePattern]
    | AstNode.Binding(SynBinding(_, _, _, _, _, _, _, pattern, _, _, _, _)) ->
        match pattern with
        | SynPat.LongIdent(LongIdentWithDots(identifiers, _),_, _, SynArgPats.Pats(names), _, _) ->
            match identifiers with
            | head::_  ->
                let result: (Ident * string * (unit -> bool) option) array = getParameterWithBelowMinimumLength names
                if isIdentifierTooShort head.idText then
                    Array.append result (Array.singleton (head, head.idText, None))  
                else 
                    result
            | _ -> Array.empty
        | SynPat.Named(_, identifier, _, _, _) when isIdentifierTooShort identifier.idText ->
            (identifier, identifier.idText, None) |> Array.singleton
        | _ -> Array.empty
    | AstNode.Field(SynField(_, _, Some identifier, _, _, _, _, _)) when isIdentifierTooShort identifier.idText ->
        (identifier, identifier.idText, None) |> Array.singleton
    | AstNode.TypeDefinition(SynTypeDefn(componentInfo, _typeDef, _, _, _)) ->
        let checkTypes types =
            seq {
                for SynTyparDecl(_attr, SynTypar(id, _, _)) in types do
                    if isIdentifierTooShort id.idText then
                        yield (id, id.idText, None)
            }
            
        match componentInfo with
        | SynComponentInfo(_attrs, types, _, _identifier, _, _, _, _) ->
            checkTypes types |> Array.ofSeq
    | AstNode.Type(SynType.Var(SynTypar(id, _, _), _)) when isIdentifierTooShort id.idText ->
        (id, id.idText, None) |> Array.singleton
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
