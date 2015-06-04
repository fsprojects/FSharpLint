(*
    FSharpLint, a linter for F#.
    Copyright (C) 2014 Matthew Mcveigh

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

namespace FSharpLint.Rules

/// Rules to enforce the use of XML documentation in various places.
module XmlDocumentation =

    open System
    open Microsoft.FSharp.Compiler.Ast
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadVisitors

    [<Literal>]
    let AnalyserName = "XmlDocumentation"

    let isAccessEnabled setting access =
        match setting, access with
            | Access(Access.Public),     SynAccess.Public
            | Access(Access.Internal),   SynAccess.Internal
            | Access(Access.Private),    SynAccess.Private
            | Access(Access.All),        SynAccess.Public
            | Access(Access.All),        SynAccess.Internal
            | Access(Access.All),        SynAccess.Private
            | Access(Access.NotPrivate), SynAccess.Public
            | Access(Access.NotPrivate), SynAccess.Internal
            | Access(Access.NotPublic),  SynAccess.Internal
            | Access(Access.NotPublic),  SynAccess.Private -> true
            | Access(Access.None), _ -> false
            | _ -> false

    let isAccessEnabledOpt setting access =
        match setting, access with
            | Some(set), Some(acc) -> isAccessEnabled set acc
            | Some(Access(Access.None)), _ -> false
            | Some(_), _ -> true // any except none
            | _ -> false

    let getSettings config name =
        match isRuleEnabled config AnalyserName name with
            | Some(_, ruleSettings)  -> Some ruleSettings
            | None -> None

    let getAccessSetting config name =
        match getSettings config name with
        | Some(ruleSettings) when ruleSettings.ContainsKey "Access" -> Some ruleSettings.["Access"]
        | _ -> None

    let configAccessExceptionHeader config access name =
        match access, getSettings config name with
        | Some acc, Some(ruleSettings) when ruleSettings.ContainsKey "Access" -> isAccessEnabled ruleSettings.["Access"] acc
        | None, Some(ruleSettings) when ruleSettings.ContainsKey "Access" -> isAccessEnabledOpt (Some ruleSettings.["Access"]) None
        | _, _ -> false

    let configExceptionHeader config name =
        match getSettings config name with
            | Some(ruleSettings) when ruleSettings.ContainsKey "Access" ->
                match ruleSettings.["Access"] with
                    | Access(Access.Public)
                    | Access(Access.Internal)
                    | Access(Access.Private)
                    | Access(Access.All)
                    | Access(Access.NotPrivate)
                    | Access(Access.NotPublic) -> true
                    | Access(Access.None) -> false
                    | _ -> false
            | Some(_)
            | None -> false

    let isPreXmlDocEmpty (preXmlDoc:PreXmlDoc) =
        match preXmlDoc.ToXmlDoc() with
            | XmlDoc(xs) ->
                let atLeastOneThatHasText ars = ars |> Array.exists (fun s -> not (String.IsNullOrWhiteSpace(s))) |> not
                xs |> atLeastOneThatHasText

    let getString str = FSharpLint.Framework.Resources.GetString(str)

    let ruleAccessEnabled visitorInfo (astNode:CurrentNode) access ruleName =
        configAccessExceptionHeader visitorInfo.Config access ruleName &&
            astNode.IsSuppressed(AnalyserName, ruleName) |> not

    let ruleEnabled visitorInfo (astNode:CurrentNode) ruleName =
        configExceptionHeader visitorInfo.Config ruleName &&
            astNode.IsSuppressed(AnalyserName, ruleName) |> not

    let getIdText (id:Ident option) =
        match id with
        | None -> ""
        | Some i -> " " + i.idText

    let visitor visitorInfo checkFile astNode =
        match astNode.Node with
        | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_, _, _, xmlDoc, _, access, range)) ->
            if ruleAccessEnabled visitorInfo astNode access "ModuleDefinitionHeader" && isPreXmlDocEmpty xmlDoc then
                visitorInfo.PostError range (getString "RulesXmlDocumentationModuleError")

        | AstNode.ExceptionRepresentation(SynExceptionRepr.ExceptionDefnRepr(_, _, _, xmlDoc, access, range)) ->
            if ruleAccessEnabled visitorInfo astNode access "ExceptionDefinitionHeader" && isPreXmlDocEmpty xmlDoc then
                visitorInfo.PostError range (getString "RulesXmlDocumentationExceptionError")

        | AstNode.EnumCase(SynEnumCase.EnumCase(_, id, _, xmlDoc, range)) ->
            if ruleEnabled visitorInfo astNode "EnumDefinitionHeader" && isPreXmlDocEmpty xmlDoc then
                visitorInfo.PostError range (String.Format(getString "RulesXmlDocumentationEnumError", id.idText))

        | AstNode.UnionCase(SynUnionCase.UnionCase(_, id, _, xmlDoc, access, range)) ->
            if ruleAccessEnabled visitorInfo astNode access "UnionDefinitionHeader" && isPreXmlDocEmpty xmlDoc then
                visitorInfo.PostError range (String.Format(getString "RulesXmlDocumentationUnionError", id.idText))

        | AstNode.MemberDefinition(SynMemberDefn.AutoProperty(_, _, id, _, _, _, xmlDoc, access, _, rangeOpt, range)) ->
            if ruleAccessEnabled visitorInfo astNode access "AutoPropertyDefinitionHeader" && isPreXmlDocEmpty xmlDoc then
                visitorInfo.PostError range (String.Format(getString "RulesXmlDocumentationAutoPropertyError", id.idText))

        | AstNode.MemberDefinition(SynMemberDefn.Member(synBinding, _)) ->
            if ruleEnabled visitorInfo astNode "MemberDefinitionHeader" then
                let (SynBinding.Binding(access, _, _, _, _, xmlDoc, _, _, _, _, range, _)) = synBinding
                let setting = getAccessSetting visitorInfo.Config "MemberDefinitionHeader"
                if isAccessEnabledOpt setting access && isPreXmlDocEmpty xmlDoc then
                    visitorInfo.PostError range (getString "RulesXmlDocumentationMemberError")

        | AstNode.MemberDefinition(SynMemberDefn.LetBindings(synBindings, _, _, range)) ->
            if ruleEnabled visitorInfo astNode "LetDefinitionHeader" then
                let setting = getAccessSetting visitorInfo.Config "LetDefinitionHeader"
                let evalBinding (SynBinding.Binding(access, _, _, _, _, xmlDoc, _, _, _, _, range, _)) =
                    if isAccessEnabledOpt setting access && isPreXmlDocEmpty xmlDoc then
                        visitorInfo.PostError range (getString "RulesXmlDocumentationLetError")
                synBindings |> List.iter evalBinding

        | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(coreInfo, typeDefnRep, _, rng)) ->
            if ruleEnabled visitorInfo astNode "TypeDefinitionHeader" then
                let (SynComponentInfo.ComponentInfo(_, _, _, _, xmlDoc, _, access, range)) = coreInfo
                let setting = getAccessSetting visitorInfo.Config "TypeDefinitionHeader"
                if isAccessEnabledOpt setting access && isPreXmlDocEmpty xmlDoc then
                    visitorInfo.PostError range (getString "RulesXmlDocumentationTypeError")

            if ruleEnabled visitorInfo astNode "RecordDefinitionHeader" then
                let setting = getAccessSetting visitorInfo.Config "RecordDefinitionHeader"
                let evalField (SynField.Field(_, _, id, _, _, xmlDoc, access, range)) =
                    if isAccessEnabledOpt setting access && isPreXmlDocEmpty xmlDoc then
                        visitorInfo.PostError range (String.Format(getString "RulesXmlDocumentationRecordError", getIdText id))
                match typeDefnRep with
                | Simple(simple, range) ->
                    match simple with
                    | SynTypeDefnSimpleRepr.Record(_, fields, _) -> fields |> List.iter evalField
                    | _ -> ()
                | _ -> ()
        | _ -> ()

        Continue

    type RegisterXmlDocumentationVisitor() =
        let plugin =
            {
                Name = AnalyserName
                Visitor = Ast(visitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin