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

namespace MattMcveigh.FSharpLint

module NameConventionRules =

    open System
    open System.Text.RegularExpressions
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open ErrorHandling
    open AstVisitorBase

    let isPascalCase (identifier:Ident) = Regex.Match(identifier.idText, @"^[A-Z]([a-z]|[A-Z]|\d)*").Success

    let isCamelCase (identifier:Ident) = Regex.Match(identifier.idText, @"^[a-z]([a-z]|[A-Z]|\d)*").Success

    let containsUnderscore (identifier:Ident) = identifier.idText.Contains("_")

    let pascalCaseError (identifier:string) = 
        String.Format("Expected pascal case identifier but was {0}", identifier)

    let camelCaseError (identifier:string) = 
        String.Format("Expected camel case identifier but was {0}", identifier)

    let isSymbolMember = function
    | Some(symbol:FSharpSymbol) ->
        match symbol with
        | :? FSharpMemberFunctionOrValue as memberFunctionOrValue when memberFunctionOrValue.IsMember -> true
        | _ -> false
    | None -> false

    let isSymbolInterface = function
    | Some(symbol:FSharpSymbol) ->
        match symbol with
        | :? FSharpEntity as entity when entity.IsInterface -> true
        | _ -> false
    | None -> false

    let namingConventionVisitor postError checkFile = 
        { new AstVisitorBase(checkFile) with
            member this.VisitModuleOrNamespace(identifier, _, _, _, _, _, range) = 
                let expect identifier =
                    if not <| isPascalCase identifier then
                        postError identifier.idRange (pascalCaseError <| identifier.idText)

                identifier |> List.iter expect

                Continue

            member this.VisitUnionCase(_, identifier, _, _, _, range) = 
                if not <| isPascalCase identifier then
                    postError identifier.idRange (pascalCaseError <| identifier.idText)

                Continue

            member this.VisitNamedPattern(_, identifier, _, _, range) = 
                if not <| isCamelCase identifier then
                    postError identifier.idRange (camelCaseError <| identifier.idText)

                Continue

            member this.VisitIdPattern(identifier, range) = 
                if not <| isCamelCase identifier then
                    postError identifier.idRange (camelCaseError <| identifier.idText)

                Continue

            member this.VisitField(_, identifier, _, _, _, range) = 
                let expected identifier =
                    if not <| isCamelCase identifier then
                        postError identifier.idRange (camelCaseError <| identifier.idText)

                identifier |> Option.iter expected

                Continue

            member this.VisitEnumCase(_, identifier, _, _, range) = 
                if not <| isPascalCase identifier then
                    postError identifier.idRange (pascalCaseError <| identifier.idText)

                Continue

            member this.VisitComponentInfo(_, _, _, identifier, _, _, range) = 
                let expect identifier =
                    if not <| isPascalCase identifier then
                        postError identifier.idRange (pascalCaseError <| identifier.idText)

                identifier |> List.iter expect

                let interfaceIdentifier = identifier.Head
                let interfaceRange = interfaceIdentifier.idRange

                let startLine, endColumn = interfaceRange.StartLine, interfaceRange.EndColumn

                if isSymbolInterface <| checkFile.GetSymbolAtLocation(startLine-1, endColumn, "", [interfaceIdentifier.idText]) then 
                    if not <| interfaceIdentifier.idText.StartsWith("I") then
                        let error = "Interface identifiers should begin with the letter I found interface " + interfaceIdentifier.idText
                        postError interfaceRange error
                    
                Continue

            member this.VisitLongIdentPattern(longIdentifier, identifier, access, range) =  
                let expect identifier =
                    if not <| isCamelCase identifier then
                        postError identifier.idRange (camelCaseError <| identifier.idText)

                let memberName = longIdentifier.Lid.[(longIdentifier.Lid.Length - 1)]

                let line, endColumn, memberIdent = memberName.idRange.EndLine, memberName.idRange.EndColumn, memberName.idText

                if checkFile.GetSymbolAtLocation(line-1, 42, "", [memberIdent]) |> isSymbolMember then
                    if not <| isPascalCase memberName then
                        postError memberName.idRange (pascalCaseError <| memberIdent)
                else
                    longIdentifier.Lid |> List.iter expect

                Continue
        }