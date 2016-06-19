// FSharpLint, a linter for F#.
// Copyright (C) 2016 Matthew Mcveigh, Jon Hamm
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace FSharpLint.Rules

/// Rules to enforce the use of XML documentation in various places.
module XmlDocumentation =
    
    open System
    open Microsoft.FSharp.Compiler.Ast
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration

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
        | Access(Access.None), _
        | _ -> false

    let isAccessEnabledOpt setting access =
        match setting, access with
        | Some(set), Some(acc) -> isAccessEnabled set acc
        | Some(Access(Access.None)), _ -> false
        | Some(_), _ -> true // any except none
        | _ -> false

    let getSettings config name =
        isRuleEnabled config AnalyserName name
        |> Option.bind (snd >> Some)

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
        | Some(_) | None -> false

    let isPreXmlDocEmpty (preXmlDoc:PreXmlDoc) =
        match preXmlDoc.ToXmlDoc() with
        | XmlDoc(xs) ->
            let atLeastOneThatHasText ars = ars |> Array.exists (String.IsNullOrWhiteSpace >> not) |> not
            xs |> atLeastOneThatHasText

    let getString = Resources.GetString

    let getIdText (id:Ident option) =
        match id with
        | None -> ""
        | Some i -> " " + i.idText
        
    let analyser visitorInfo _ syntaxArray skipArray = 
        let isNotSuppressed ruleName i =
            AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
            |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName
            |> not

        let ruleAccessEnabled visitorInfo i access ruleName =
            configAccessExceptionHeader visitorInfo.Config access ruleName && isNotSuppressed ruleName i

        let ruleEnabled visitorInfo i ruleName =
            configExceptionHeader visitorInfo.Config ruleName && isNotSuppressed ruleName i

        let mutable i = 0
        while i < syntaxArray.Length do
            match syntaxArray.[i].Actual with
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_, _, _, xmlDoc, _, access, range)) ->
                if ruleAccessEnabled visitorInfo i access "ModuleDefinitionHeader" && isPreXmlDocEmpty xmlDoc then
                    visitorInfo.PostError range (getString "RulesXmlDocumentationModuleError")

            | AstNode.ExceptionRepresentation(SynExceptionRepr.ExceptionDefnRepr(_, _, _, xmlDoc, access, range)) ->
                if ruleAccessEnabled visitorInfo i access "ExceptionDefinitionHeader" && isPreXmlDocEmpty xmlDoc then
                    visitorInfo.PostError range (getString "RulesXmlDocumentationExceptionError")

            | AstNode.EnumCase(SynEnumCase.EnumCase(_, id, _, xmlDoc, range)) ->
                if ruleEnabled visitorInfo i "EnumDefinitionHeader" && isPreXmlDocEmpty xmlDoc then
                    visitorInfo.PostError range (String.Format(getString "RulesXmlDocumentationEnumError", id.idText))

            | AstNode.UnionCase(SynUnionCase.UnionCase(_, id, _, xmlDoc, access, range)) ->
                if ruleAccessEnabled visitorInfo i access "UnionDefinitionHeader" && isPreXmlDocEmpty xmlDoc then
                    visitorInfo.PostError range (String.Format(getString "RulesXmlDocumentationUnionError", id.idText))

            | AstNode.MemberDefinition(SynMemberDefn.AutoProperty(_, _, id, _, _, _, xmlDoc, access, _, _, range)) ->
                if ruleAccessEnabled visitorInfo i access "AutoPropertyDefinitionHeader" && isPreXmlDocEmpty xmlDoc then
                    visitorInfo.PostError range (String.Format(getString "RulesXmlDocumentationAutoPropertyError", id.idText))

            | AstNode.MemberDefinition(SynMemberDefn.Member(synBinding, _)) ->
                if ruleEnabled visitorInfo i "MemberDefinitionHeader" then
                    let (SynBinding.Binding(access, _, _, _, _, xmlDoc, _, _, _, _, range, _)) = synBinding
                    let setting = getAccessSetting visitorInfo.Config "MemberDefinitionHeader"
                    if isAccessEnabledOpt setting access && isPreXmlDocEmpty xmlDoc then
                        visitorInfo.PostError range (getString "RulesXmlDocumentationMemberError")

            | AstNode.MemberDefinition(SynMemberDefn.LetBindings(synBindings, _, _, _)) ->
                if ruleEnabled visitorInfo i "LetDefinitionHeader" then
                    let setting = getAccessSetting visitorInfo.Config "LetDefinitionHeader"
                    let evalBinding (SynBinding.Binding(access, _, _, _, _, xmlDoc, _, _, _, _, range, _)) =
                        if isAccessEnabledOpt setting access && isPreXmlDocEmpty xmlDoc then
                            visitorInfo.PostError range (getString "RulesXmlDocumentationLetError")
                    synBindings |> List.iter evalBinding

            | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(coreInfo, typeDefnRep, _, _)) ->
                if ruleEnabled visitorInfo i "TypeDefinitionHeader" then
                    let (SynComponentInfo.ComponentInfo(_, _, _, _, xmlDoc, _, access, range)) = coreInfo
                    let setting = getAccessSetting visitorInfo.Config "TypeDefinitionHeader"
                    if isAccessEnabledOpt setting access && isPreXmlDocEmpty xmlDoc then
                        visitorInfo.PostError range (getString "RulesXmlDocumentationTypeError")

                if ruleEnabled visitorInfo i "RecordDefinitionHeader" then
                    let setting = getAccessSetting visitorInfo.Config "RecordDefinitionHeader"
                    let evalField (SynField.Field(_, _, id, _, _, xmlDoc, access, range)) =
                        if isAccessEnabledOpt setting access && isPreXmlDocEmpty xmlDoc then
                            visitorInfo.PostError range (String.Format(getString "RulesXmlDocumentationRecordError", getIdText id))
                    match typeDefnRep with
                    | Simple(SynTypeDefnSimpleRepr.Record(_, fields, _), _) ->
                        fields |> List.iter evalField
                    | _ -> ()
            | _ -> ()

            i <- i + 1