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

module Program =

    open Ast
    open ErrorHandling
    open NameConventionRules
        
    [<EntryPoint>]
    let main argv = 
        let input = """
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

/// Checks whether any code in an F# program violates best practices for naming identifiers.
module NameConventionRules =

    open System
    open System.Linq
    open System.Text.RegularExpressions
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open AstVisitorBase
    open ErrorHandling

    let isPascalCase (identifier:string) = Regex.Match(identifier, @"^[A-Z]([a-z]|[A-Z]|\d)*").Success

    let isCamelCase (identifier:string) = Regex.Match(identifier, @"^[a-z]([a-z]|[A-Z]|\d)*").Success

    let containsUnderscore (identifier:string) = identifier.Contains("_")

    let pascalCaseError (identifier:string) = 
        String.Format("Expected pascal case identifier but was {0}", identifier)

    let camelCaseError (identifier:string) = 
        String.Format("Expected camel case identifier but was {0}", identifier)

    let expectNoUnderscore postError (identifier:Ident) =
        if containsUnderscore identifier.idText then
            let error = String.Format("Identifiers should not contain underscores, but one was found in {0}", identifier)
            postError identifier.idRange error

    let expect predicate getError postError (identifier:Ident) =
        expectNoUnderscore postError identifier

        if not <| predicate identifier.idText then
            let error = getError identifier.idText
            postError identifier.idRange error

    let expectCamelCase = expect isCamelCase camelCaseError

    let expectPascalCase = expect isPascalCase pascalCaseError

    let isSymbolInterface = function
    | Some(symbol:FSharpSymbol) ->
        match symbol with
        | :? FSharpEntity as entity when entity.IsInterface -> true
        | _ -> false
    | None -> false

    type IsProperty =
    | Property of range
    | NotProperty

    let isIdentifierAProperty = function
    | Some(identifier:Ident) ->
        match identifier.idText with
        | "set" | "get" -> Property identifier.idRange
        | _ ->  NotProperty
    | _ ->  NotProperty

    let expectValidActivePatternDefinition postError (identifier:Ident) =
        let error ident =
            if containsUnderscore ident then
                let error = String.Format("Identifiers should not contain underscores, but one was found in: {0}", ident)
                postError identifier.idRange error

        identifier.idText.Split([|'|'|]).Where(fun x -> not <| String.IsNullOrEmpty(x) && x.Trim() <> "_")
            |> Seq.iter error

    type LongIdentPatternType =
    | UnionCase of FSharpUnionCase
    | Member of FSharpMemberFunctionOrValue
    | ActivePatternDefinition of FSharpMemberFunctionOrValue
    | ActivePatternCase of FSharpSymbol
    | ValueOrFunction of FSharpMemberFunctionOrValue
    | Other

    let longIdentPatternType (longIdentifier:LongIdentWithDots) (identifier:Ident option) (checkFile:CheckFileResults) = 
        let name = longIdentifier.Lid.[(longIdentifier.Lid.Length - 1)]

        let range = match isIdentifierAProperty identifier with
                    | Property(range) -> range
                    | NotProperty -> name.idRange

        let line, endColumn, ident = range.EndLine, range.EndColumn, name.idText

        let symbol = checkFile.GetSymbolAtLocation(line - 1, endColumn, "", [ident])
        
        match symbol with
        | Some(symbol) ->
            match symbol with
            | :? FSharpUnionCase as Unioncase -> UnionCase Unioncase
            | :? FSharpMemberFunctionOrValue as memberFunctionOrValue -> 
                if memberFunctionOrValue.IsMember then
                    Member memberFunctionOrValue
                else if memberFunctionOrValue.IsActivePattern then
                    ActivePatternDefinition memberFunctionOrValue
                else 
                    ValueOrFunction memberFunctionOrValue
            | _ -> Other
        | None -> 
            Other

    type Cats = { Result: string; cats: int list }

    let Dog = 0

    /// Gets a visitor that checks all nodes on the AST where an identifier may be declared, 
    /// and post errors if any violate best practice guidelines.
    let namingConventionVisitor postError checkFile = 
        let expectCamelCase, expectPascalCase = expectCamelCase postError, expectPascalCase postError

        let Meow = 0

        { new AstVisitorBase(checkFile) with
            member this.VisitModuleOrNamespace(identifier, _, _, _, _, _, range) = 
                let Cat () = ()

                match 0 with | 0 as Goat -> () | _ -> ()

                let Goat = 0
                identifier |> List.iter expectPascalCase
                Continue

            member this.VisitUnionCase(_, identifier, _, _, _, range) = 
                expectPascalCase identifier
                Continue

            member this.VisitNamedPattern(_, identifier, _, _, range) = 
                expectCamelCase identifier
                Continue

            member this.VisitIdPattern(identifier, range) = 
                expectCamelCase identifier
                Continue

            member this.VisitField(_, identifier, _, _, _, range) = 
                identifier |> Option.iter expectPascalCase
                Continue

            member this.VisitEnumCase(_, identifier, _, _, range) = 
                expectPascalCase identifier
                Continue

            member this.VisitComponentInfo(_, _, _, identifier, _, _, range) = 
                identifier |> List.iter expectPascalCase

                let interfaceIdentifier = identifier.Head
                let interfaceRange = interfaceIdentifier.idRange

                let startLine, endColumn = interfaceRange.StartLine, interfaceRange.EndColumn

                if isSymbolInterface <| checkFile.GetSymbolAtLocation(startLine - 1, endColumn, "", [interfaceIdentifier.idText]) then 
                    if not <| interfaceIdentifier.idText.StartsWith("I") then
                        let error = "Interface identifiers should begin with the letter I found interface " + interfaceIdentifier.idText
                        postError interfaceRange error
                    
                Continue

            member this.VisitLongIdentPattern(longIdentifier, identifier, access, range) =  
                let lastIdent = longIdentifier.Lid.[(longIdentifier.Lid.Length - 1)]

                match longIdentPatternType longIdentifier identifier checkFile with
                | Member(_) -> expectPascalCase lastIdent
                | ValueOrFunction(_) -> expectCamelCase lastIdent
                | ActivePatternDefinition(_) -> expectValidActivePatternDefinition postError lastIdent
                | _ -> ()

                let Cat = ()

                Continue

            member this.VisitExceptionRepresentation(_, unionCase, _, _, _, _) = 
                match unionCase with 
                | SynUnionCase.UnionCase(_, identifier, _, _, _, _) ->
                    if not <| identifier.idText.EndsWith("Exception") then
                        let error = String.Format("Exception identifier should end with 'Exception', but was {0}", identifier)
                        postError identifier.idRange error
                
                Continue

            /// If inside of an implementation file the node must be an abstract member signature.
            member this.VisitValueSignature(identifier, range) = 
                expectPascalCase identifier
                Continue
        }
        """

        let postError range error =
            errorHandler.Post(
                {
                    info = error
                    range = range
                    input = input
                })

        let visitor = namingConventionVisitor postError

        parse "/home/user/Dog.test.fsx" input [visitor] |> ignore

        System.Console.ReadKey() |> ignore

        0