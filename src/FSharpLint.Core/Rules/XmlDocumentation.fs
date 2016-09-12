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
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Ast

    module internal XmlDocConfig =

        open FSharpLint.Framework.Configuration
    
        let private isAccessEnabled setting access =
            match setting, access with
            | Access(Access.Public),     SynAccess.Public
            | Access(Access.Internal),   SynAccess.Internal
            | Access(Access.Private),    SynAccess.Private
            | Access(Access.All),        (SynAccess.Public | SynAccess.Internal | SynAccess.Private)
            | Access(Access.NotPrivate), (SynAccess.Public | SynAccess.Internal)
            | Access(Access.NotPublic),  (SynAccess.Internal | SynAccess.Private) -> true
            | Access(Access.None), _
            | _ -> false

        let isAccessEnabledOpt setting access =
            match setting, access with
            | Some(set), Some(acc) -> isAccessEnabled set acc
            | Some(Access(Access.None)), _ -> false
            | Some(_), _ -> true // any except none
            | _ -> false
        
    type XmlDocRule(analyserName, name, code, ruleConfig:Configuration.Configuration) =
        inherit Rule(analyserName, name, code, ruleConfig)

        let atLeastOneHasText = Array.exists (String.IsNullOrWhiteSpace >> not)

        let isPreXmlDocEmpty (preXmlDoc:PreXmlDoc) =
            let (XmlDoc xs) = preXmlDoc.ToXmlDoc()
            xs |> atLeastOneHasText |> not

        let accessSetting = 
            ruleConfig.Analysers.TryFind analyserName
            |> Option.bind (fun x -> x.Rules.TryFind name)
            |> Option.bind (fun x -> x.Settings.TryFind "Access")

        member this.IsRuleBroken(xmlDoc:PreXmlDoc, access:SynAccess option, analysisArgs, i) = 
            this.Enabled
                && XmlDocConfig.isAccessEnabledOpt accessSetting access
                && isPreXmlDocEmpty xmlDoc
                && this.NotSuppressed analysisArgs i

    module Analysis =
        type IXmlDocumentationAnalyser =
            abstract member ModuleDefinitionXmlDoc: XmlDocRule
            abstract member ExceptionDefinitionXmlDoc: XmlDocRule
            abstract member EnumDefinitionXmlDoc: XmlDocRule
            abstract member UnionDefinitionXmlDoc: XmlDocRule
            abstract member AutoPropertyDefinitionXmlDoc: XmlDocRule
            abstract member MemberDefinitionXmlDoc: XmlDocRule
            abstract member LetDefinitionXmlDoc: XmlDocRule
            abstract member TypeDefinitionXmlDoc: XmlDocRule
            abstract member RecordDefinitionXmlDoc: XmlDocRule

        let private getIdText (id:Ident option) =
            match id with
            | None -> ""
            | Some i -> " " + i.idText
        
        let analyse (analyser:IXmlDocumentationAnalyser) analysisArgs = 
            let mutable i = 0
            while i < analysisArgs.SyntaxArray.Length do
                match analysisArgs.SyntaxArray.[i].Actual with
                | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_, _, _, _, xmlDoc, _, access, range)) ->
                    if analyser.ModuleDefinitionXmlDoc.IsRuleBroken(xmlDoc, access, analysisArgs, i) then
                        analyser.ModuleDefinitionXmlDoc.MessageFormat()
                        |> analysisArgs.Context.PostError range

                | AstNode.ExceptionRepresentation(SynExceptionDefnRepr.SynExceptionDefnRepr(_, _, _, xmlDoc, access, range)) ->
                    if analyser.ExceptionDefinitionXmlDoc.IsRuleBroken(xmlDoc, access, analysisArgs, i) then
                        analyser.ExceptionDefinitionXmlDoc.MessageFormat()
                        |> analysisArgs.Context.PostError range

                | AstNode.EnumCase(SynEnumCase.EnumCase(_, id, _, xmlDoc, range)) ->
                    if analyser.EnumDefinitionXmlDoc.IsRuleBroken(xmlDoc, None, analysisArgs, i) then
                        analyser.EnumDefinitionXmlDoc.MessageFormat(id.idText)
                        |> analysisArgs.Context.PostError range

                | AstNode.UnionCase(SynUnionCase.UnionCase(_, id, _, xmlDoc, access, range)) ->
                    if analyser.UnionDefinitionXmlDoc.IsRuleBroken(xmlDoc, access, analysisArgs, i) then
                        analyser.UnionDefinitionXmlDoc.MessageFormat(id.idText)
                        |> analysisArgs.Context.PostError range

                | AstNode.MemberDefinition(SynMemberDefn.AutoProperty(_, _, id, _, _, _, xmlDoc, access, _, _, range)) ->
                    if analyser.AutoPropertyDefinitionXmlDoc.IsRuleBroken(xmlDoc, access, analysisArgs, i) then
                        analyser.AutoPropertyDefinitionXmlDoc.MessageFormat(id.idText)
                        |> analysisArgs.Context.PostError range

                | AstNode.MemberDefinition(SynMemberDefn.Member(synBinding, _)) ->
                    if analyser.MemberDefinitionXmlDoc.Enabled then
                        let (SynBinding.Binding(access, _, _, _, _, xmlDoc, _, _, _, _, range, _)) = synBinding
                        if analyser.MemberDefinitionXmlDoc.IsRuleBroken(xmlDoc, access, analysisArgs, i) then
                            analyser.MemberDefinitionXmlDoc.MessageFormat()
                            |> analysisArgs.Context.PostError range

                | AstNode.MemberDefinition(SynMemberDefn.LetBindings(synBindings, _, _, _)) ->
                    if analyser.LetDefinitionXmlDoc.Enabled then
                        let evalBinding (SynBinding.Binding(access, _, _, _, _, xmlDoc, _, _, _, _, range, _)) =
                            if analyser.LetDefinitionXmlDoc.IsRuleBroken(xmlDoc, access, analysisArgs, i) then
                                analyser.LetDefinitionXmlDoc.MessageFormat()
                                |> analysisArgs.Context.PostError range
                        synBindings |> List.iter evalBinding

                | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(coreInfo, typeDefnRep, _, _)) ->
                    if analyser.TypeDefinitionXmlDoc.Enabled then
                        let (SynComponentInfo.ComponentInfo(_, _, _, _, xmlDoc, _, access, range)) = coreInfo
                        if analyser.TypeDefinitionXmlDoc.IsRuleBroken(xmlDoc, access, analysisArgs, i) then
                            analyser.TypeDefinitionXmlDoc.MessageFormat()
                            |> analysisArgs.Context.PostError range

                    if analyser.RecordDefinitionXmlDoc.Enabled then
                        let evalField (SynField.Field(_, _, id, _, _, xmlDoc, access, range)) =
                            if analyser.RecordDefinitionXmlDoc.IsRuleBroken(xmlDoc, access, analysisArgs, i) then
                                analyser.RecordDefinitionXmlDoc.MessageFormat(getIdText id)
                                |> analysisArgs.Context.PostError range
                        match typeDefnRep with
                        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, fields, _), _) ->
                            fields |> List.iter evalField
                        | _ -> ()
                | _ -> ()

                i <- i + 1

    [<Sealed>]
    type XmlDocumentationAnalyser(config) =
        inherit Analyser.Analyser(name = "XmlDocumentation", code = "8", config = config)

        interface Analysis.IXmlDocumentationAnalyser with
            member this.ModuleDefinitionXmlDoc = 
                XmlDocRule(this.Name, "ModuleDefinitionXmlDoc", "1", config)

            member this.ExceptionDefinitionXmlDoc = 
                XmlDocRule(this.Name, "ExceptionDefinitionXmlDoc", "2", config)

            member this.EnumDefinitionXmlDoc = 
                XmlDocRule(this.Name, "EnumDefinitionXmlDoc", "3", config)

            member this.UnionDefinitionXmlDoc = 
                XmlDocRule(this.Name, "UnionDefinitionXmlDoc", "4", config)

            member this.AutoPropertyDefinitionXmlDoc = 
                XmlDocRule(this.Name, "AutoPropertyDefinitionXmlDoc", "5", config)

            member this.MemberDefinitionXmlDoc = 
                XmlDocRule(this.Name, "MemberDefinitionXmlDoc", "6", config)

            member this.LetDefinitionXmlDoc = 
                XmlDocRule(this.Name, "LetDefinitionXmlDoc", "7", config)

            member this.TypeDefinitionXmlDoc = 
                XmlDocRule(this.Name, "TypeDefinitionXmlDoc", "8", config)

            member this.RecordDefinitionXmlDoc = 
                XmlDocRule(this.Name, "RecordDefinitionXmlDoc", "9", config)

        override this.Analyse analysisArgs = 
            Analysis.analyse this analysisArgs