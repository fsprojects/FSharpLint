module FSharpLint.Rules.MaxNumberOfMembers

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let rule config =
    let runner (config:Helper.NumberOfItems.Config) (args:AstNodeRuleParams) =
        let getMembers (members:SynMemberDefn list) =
            let isPublic = function
                | Some(SynAccess.Public(_)) | None -> true
                | Some(_) -> false

            let isPublicMember = function
                | SynMemberDefn.AbstractSlot(_) -> true
                | SynMemberDefn.Member(SynBinding(access, _, _, _, _, _, _, _, _, _, _, _, _), _)
                | SynMemberDefn.AutoProperty(_, _, _, _, _, _, _, _, SynValSigAccess.Single (access), _, _, _) -> isPublic access
                | SynMemberDefn.AutoProperty(_, _, _, _, _, _, _, _, SynValSigAccess.GetSet (access, _, _), _, _, _) -> isPublic access
                | _ -> false

            List.filter isPublicMember members

        let validateType (maxMembers:int) members typeRepresentation =
            let members =
                match typeRepresentation with
                | SynTypeDefnRepr.Simple(_) | SynTypeDefnRepr.Exception(_) -> members
                | SynTypeDefnRepr.ObjectModel(_, members, _) -> getMembers members

            if List.length members > maxMembers then
                let errorFormatString = Resources.GetString("RulesNumberOfItemsClassMembersError")
                let error = String.Format(errorFormatString, maxMembers)
                Array.singleton
                    {
                        Range = members.[maxMembers].Range
                        Message = error
                        SuggestedFix = None
                        TypeChecks = List.Empty
                    }
            else
                Array.empty

        match args.AstNode with
        | AstNode.TypeDefinition(SynTypeDefn(_, typeRepresentation, members, implicitCtor, _, _)) ->
            validateType config.MaxItems (Option.toList implicitCtor @ members) typeRepresentation
        | _ -> Array.empty

    AstNodeRule
        {
            Name = "MaxNumberOfMembers"
            Identifier = Identifiers.MaxNumberOfMembers
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
