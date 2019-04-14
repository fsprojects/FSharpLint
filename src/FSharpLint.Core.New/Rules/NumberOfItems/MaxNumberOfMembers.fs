module FSharpLint.Rules.MaxNumberOfMembers

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Ast
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

[<RequireQualifiedAccess>]
type Config =
    { maxMembers : int }

let private getMembers (members:SynMemberDefn list) =
    let isPublic = function
        | Some(SynAccess.Public) | None -> true
        | Some(_) -> false

    let isPublicMember = function
        | SynMemberDefn.AbstractSlot(_) -> true
        | SynMemberDefn.Member(SynBinding.Binding(access, _, _, _, _, _, _, _, _, _, _, _), _)
        | SynMemberDefn.AutoProperty(_, _, _, _, _, _, _, access, _, _, _) -> isPublic access
        | _ -> false

    members
    |> List.filter isPublicMember

let private validateType (maxMembers:int) members typeRepresentation =
    let members =
        match typeRepresentation with
        | SynTypeDefnRepr.Simple(_) | SynTypeDefnRepr.Exception(_) -> members
        | SynTypeDefnRepr.ObjectModel(_, members, _) -> members
        |> getMembers

    if List.length members > maxMembers then
        let errorFormatString = Resources.GetString("RulesNumberOfItemsClassMembersError")
        let error = String.Format(errorFormatString, maxMembers)
        { Range = members.[maxMembers].Range; Message = error; SuggestedFix = None; TypeChecks = [] } |> Array.singleton
    else
        Array.empty
        
let private runner (config:Config) (args:AstNodeRuleParams) =
    match args.astNode with
    | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(_, typeRepresentation, members, _)) ->
        validateType config.maxMembers members typeRepresentation
    | _ -> Array.empty
    
let rule config =
    { name = "MaxNumberOfMembers"
      identifier = None
      ruleConfig = { AstNodeRuleConfig.runner = runner config; cleanup = ignore } }
    |> AstNodeRule
