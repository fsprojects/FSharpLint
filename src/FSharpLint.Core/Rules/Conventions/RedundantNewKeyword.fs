module FSharpLint.Rules.RedundantNewKeyword

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private implementsIDisposable (fsharpType: FSharpType) =
    if fsharpType.HasTypeDefinition then
        match fsharpType.TypeDefinition.TryFullName with
        | Some (fullName) -> fullName = typeof<System.IDisposable>.FullName
        | None -> false
    else
        false

let private doesNotImplementIDisposable (checkFile: FSharpCheckFileResults) (ident: LongIdentWithDots) =
    fun () ->
        let names = ident.Lid |> List.map (fun x -> x.idText)

        let symbol =
            checkFile.GetSymbolUseAtLocation(ident.Range.StartLine, ident.Range.EndColumn, "", names)

        match symbol with
        | Some (symbol) when (symbol.Symbol :? FSharpMemberOrFunctionOrValue) ->
            let ctor = symbol.Symbol :?> FSharpMemberOrFunctionOrValue

            ctor.DeclaringEntity
            |> Option.exists (fun ctorForType -> Seq.forall (implementsIDisposable >> not) ctorForType.AllInterfaces)
        | Some symbol when (symbol.Symbol :? FSharpEntity) ->
            let ctor = symbol.Symbol :?> FSharpEntity

            Seq.forall (implementsIDisposable >> not) ctor.AllInterfaces
        | Some _ -> false
        | None -> true

let private generateFix (text: string) range =
    lazy
        (ExpressionUtilities.tryFindTextOfRange range text
         |> Option.map (fun fromText ->
             let withoutLeadingWhitespace = fromText.TrimStart()
             let newKeywordRemoved = withoutLeadingWhitespace.Substring(3).TrimStart()

             { FromText = fromText
               FromRange = range
               ToText = newKeywordRemoved }))


let runner args =
    match args.AstNode, args.CheckInfo with
    | AstNode.Expression (SynExpr.Upcast (SynExpr.New (_, SynType.LongIdent (identifier), _, range), _, _)),
      Some checkInfo
    | AstNode.Expression (SynExpr.New (_, SynType.LongIdent (identifier), _, range)), Some checkInfo
    | AstNode.Expression (SynExpr.New (_, SynType.App (SynType.LongIdent (identifier), _, _, _, _, _, _), _, range)),
      Some checkInfo ->
        { Range = range
          Message = Resources.GetString("RulesRedundantNewKeyword")
          SuggestedFix = Some(generateFix args.FileContent range)
          TypeChecks = [ doesNotImplementIDisposable checkInfo identifier ] }
        |> Array.singleton
    | _ -> Array.empty

let rule =
    { Name = "RedundantNewKeyword"
      Identifier = Identifiers.RedundantNewKeyword
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
