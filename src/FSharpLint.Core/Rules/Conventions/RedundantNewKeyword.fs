module FSharpLint.Rules.RedundantNewKeyword

open System

open FSharpLint.Framework
open FSharpLint.Framework.Violation
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private implementsIDisposable (fsharpType:FSharpType) =
    if fsharpType.HasTypeDefinition then
        match fsharpType.TypeDefinition.TryFullName with
        | Some(fullName) -> fullName = typeof<System.IDisposable>.FullName
        | None -> false
    else
        false

let private doesNotImplementIDisposable (checkFile:FSharpCheckFileResults) (ident: SynLongIdent) =
    let names = List.map (fun (identifier: Ident) -> identifier.idText) ident.LongIdent
    let symbol = checkFile.GetSymbolUseAtLocation(ident.Range.StartLine, ident.Range.EndColumn, String.Empty, names)

    match symbol with
    | Some(symbol) when (symbol.Symbol :? FSharpMemberOrFunctionOrValue) ->
        let ctor = symbol.Symbol :?> FSharpMemberOrFunctionOrValue

        Option.exists
            (fun (ctorForType: FSharpEntity) -> Seq.forall (implementsIDisposable >> not) ctorForType.AllInterfaces)
            ctor.DeclaringEntity
    | Some symbol when (symbol.Symbol :? FSharpEntity) ->
        let ctor = symbol.Symbol :?> FSharpEntity
        Seq.forall (implementsIDisposable >> not) ctor.AllInterfaces
    | Some _ -> false
    | None -> true

let private generateFix (text:string) range = lazy(
    ExpressionUtilities.tryFindTextOfRange range text
    |> Option.map (fun fromText ->
        let withoutLeadingWhitespace = fromText.TrimStart()
        let newKeywordRemoved = withoutLeadingWhitespace.Substring(3).TrimStart()
        { FromText = fromText; FromRange = range; ToText = newKeywordRemoved }))


let runner args =
    match (args.AstNode, args.CheckInfo) with
    | AstNode.Expression(SynExpr.New(_, SynType.LongIdent(identifier), _, range)), Some checkInfo
    | AstNode.Expression(SynExpr.New(_, SynType.App(SynType.LongIdent(identifier), _, _, _, _, _, _), _, range)), Some checkInfo ->
        Array.singleton
            {
                Range = range
                Message = Resources.GetString("RulesRedundantNewKeyword")
                SuggestedFix = Some(generateFix args.FileContent range)
                TypeChecks =
                    [
                        fun () -> doesNotImplementIDisposable checkInfo identifier
                    ]
            }
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "RedundantNewKeyword"
            Identifier = Identifiers.RedundantNewKeyword
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
