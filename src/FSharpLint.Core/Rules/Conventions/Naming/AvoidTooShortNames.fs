module FSharpLint.Rules.AvoidTooShortNames

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Rules.Helper

let private isIdentifierTooShort (identifier: string) =
    identifier.Length < 2 && not (identifier.StartsWith '_')

[<TailCall>]
let rec private getParameterWithBelowMinimumLengthInner patArray acc =
    match patArray with
    | [] -> acc
    | SynPat.Named(SynIdent(ident, _), _, _, _)::tail ->
        if isIdentifierTooShort ident.idText then
            Array.singleton (ident, ident.idText, None) |> Array.append acc |> getParameterWithBelowMinimumLengthInner tail
        else
            getParameterWithBelowMinimumLengthInner tail acc
    | SynPat.Paren(pat, _)::tail ->
        match pat with
        | SynPat.Typed(typedPat, _, _) ->
            getParameterWithBelowMinimumLengthInner (typedPat::tail) acc
        | SynPat.Tuple(_, elementPats, _, _) ->
            getParameterWithBelowMinimumLengthInner elementPats acc
        | _ -> getParameterWithBelowMinimumLengthInner (pat::tail) acc
    | SynPat.LongIdent(_, _, _, argPats, _, _)::tail ->
        match argPats with
        | SynArgPats.Pats(pats) ->
            getParameterWithBelowMinimumLengthInner (List.append pats tail) acc
        | _ ->
            getParameterWithBelowMinimumLengthInner tail acc
    | _ :: tail -> getParameterWithBelowMinimumLengthInner tail acc

let runner (args:AstNodeRuleParams) =
    let getParameterWithBelowMinimumLength (pats: SynPat list): (Ident * string * (unit -> bool) option) array =
        getParameterWithBelowMinimumLengthInner pats Array.empty

    let checkIdentifierPart (idText:string) =
        let formatError errorName =
            String.Format(Resources.GetString errorName, idText)

        "RulesAvoidTooShortNamesError" |> formatError |> Array.singleton

    let checkIdentifier (identifier:Ident) (idText:string) =
        if isIdentifierTooShort idText then
            checkIdentifierPart idText
            |> Array.map (fun message ->
                { Range = identifier.idRange
                  Message = message
                  SuggestedFix = None
                  TypeChecks = List.Empty })
        else
            Array.empty
    
    let identifiers =
        match args.AstNode with
        | AstNode.Expression(SynExpr.LetOrUseBang(_, _, _, pat, _, _, _, _, _)) ->
            getParameterWithBelowMinimumLength [pat]
        | AstNode.Expression(SynExpr.Lambda(_, _, lambdaArgs, _, _, _, _)) ->
            let lambdaIdent = FunctionReimplementation.getLambdaParamIdent lambdaArgs
            match lambdaIdent with
            | Some ident -> Array.singleton (ident, ident.idText, None)
            | None -> Array.empty
        | AstNode.Expression(SynExpr.ForEach(_, _, _, _, pat, _, _, _)) ->
            getParameterWithBelowMinimumLength (List.singleton pat)
        | AstNode.Expression(SynExpr.For(_, _, identifier, _, _, _, _, _, _)) when isIdentifierTooShort identifier.idText ->
            Array.singleton (identifier, identifier.idText, None)
        | AstNode.Match(SynMatchClause(namePattern, _, _, _, _, _)) ->
            getParameterWithBelowMinimumLength [namePattern]
        | AstNode.Binding(SynBinding(_, _, _, _, _, _, _, pattern, _, _, _, _, _)) ->
            match pattern with
            | SynPat.LongIdent(SynLongIdent(idents, _, _),_, _, SynArgPats.Pats(names), _, _) ->
                match idents with
                | head::_  ->
                    let result: (Ident * string * (unit -> bool) option) array = getParameterWithBelowMinimumLength names
                    if isIdentifierTooShort head.idText then
                        Array.append result (Array.singleton (head, head.idText, None))
                    else
                        result
                | _ -> Array.empty
            | SynPat.Named(SynIdent(identifier, _), _, _, _) when isIdentifierTooShort identifier.idText ->
                Array.singleton (identifier, identifier.idText, None)
            | _ -> Array.empty
        | AstNode.Field(SynField(_, _, Some identifier, _, _, _, _, _, _)) when isIdentifierTooShort identifier.idText ->
            Array.singleton (identifier, identifier.idText, None)
        | AstNode.TypeDefinition(SynTypeDefn(componentInfo, _typeDef, _, _, _, _)) ->
            let checkTypes types =
                seq {
                    for SynTyparDecl(_attr, SynTypar(id, _, _), _, _) in types do
                        if isIdentifierTooShort id.idText then
                            yield (id, id.idText, None)
                }

            match componentInfo with
            | SynComponentInfo(_attrs, maybeTypes, _, _identifier, _, _, _, _) ->
                match maybeTypes with
                | Some types -> checkTypes types.TyparDecls |> Array.ofSeq
                | None -> Array.empty
        | AstNode.Type(SynType.Var(SynTypar(id, _, _), _)) when isIdentifierTooShort id.idText ->
            Array.singleton (id, id.idText, None)
        | _ -> Array.empty

    identifiers
    |> Array.collect (fun (identifier, idText, typeCheck) ->
        let suggestions = checkIdentifier identifier idText
        Array.map (fun suggestion -> { suggestion with TypeChecks = Option.toList typeCheck }) suggestions)

let rule =
    AstNodeRule
        {
            Name = "AvoidTooShortNames"
            Identifier = Identifiers.AvoidTooShortNames
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
