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

module SourceLength =
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.TypeChecking

    let (|Member|Function|Value|Constructor|Property|) = function
        | SynValData.SynValData(memberFlags, valInfo, _) -> 
            match memberFlags with
                | Some(memberFlags) -> 
                    match memberFlags.MemberKind with
                        | MemberKind.Constructor(_)
                        | MemberKind.ClassConstructor(_) -> Constructor
                        | MemberKind.Member(_) -> Member
                        | MemberKind.PropertyGet(_)
                        | MemberKind.PropertySet(_)
                        | MemberKind.PropertyGetSet(_) -> Property
                | None when valInfo.ArgInfos.Length = 0 -> Value
                | None -> Function
        
    // Change this to be retrieved from a config file.
    [<Literal>]
    let FunctionLength = 70

    [<Literal>]
    let LambdaFunctionLength = 10

    [<Literal>]
    let ValueLength = 70

    [<Literal>]
    let MemberLength = 70

    [<Literal>]
    let ConstructorLength = 70

    [<Literal>]
    let PropertyLength = 70

    [<Literal>]
    let ClassLength = 70

    [<Literal>]
    let EnumLength = 70

    [<Literal>]
    let UnionLength = 70

    [<Literal>]
    let RecordLength = 70

    [<Literal>]
    let ModuleLength = 70

    let error name i actual = sprintf "%ss should be less than %d lines long, was %d lines long." name i actual

    let inline length (range:range) = range.EndLine - range.StartLine
    
    let rec visitor visitorInfo checkFile astNode = 
        match astNode.Node with
            | AstNode.Expression(SynExpr.Lambda(c, _, _, _, range)) -> 
                if length range > LambdaFunctionLength then
                    visitorInfo.PostError range (error "Lambda Function" LambdaFunctionLength (length range))

                Continue
            | AstNode.Binding(binding) ->
                match binding with
                    | SynBinding.Binding(_, _, _, _, attributes, _, valData, pattern, _, _, _, _) -> 
                        let length = length binding.RangeOfBindingAndRhs

                        match valData with
                            | Value when length > ValueLength -> 
                                visitorInfo.PostError binding.RangeOfBindingAndRhs (error "Value" ValueLength length)
                            | Function when length > FunctionLength -> 
                                visitorInfo.PostError binding.RangeOfBindingAndRhs (error "Function" FunctionLength length)
                            | Member when length > MemberLength -> 
                                visitorInfo.PostError binding.RangeOfBindingAndRhs (error "Member" MemberLength length)
                            | Constructor when length > ConstructorLength -> 
                                visitorInfo.PostError binding.RangeOfBindingAndRhs (error "Constructor" ConstructorLength length)
                            | Property when length > PropertyLength -> 
                                visitorInfo.PostError binding.RangeOfBindingAndRhs (error "Property" PropertyLength length)
                            | _ -> ()

                        Continue
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(identifier, isModule, _, _, _, _, range)) when isModule -> 
                if length range > FunctionLength then
                    visitorInfo.PostError range (error "Module" ModuleLength (length range))

                Continue
            | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(_, repr, _, range)) ->
                match repr with
                    | SynTypeDefnRepr.Simple(simpleRepr, _) ->
                        match simpleRepr with
                            | SynTypeDefnSimpleRepr.Record(_) -> 
                                if length range > RecordLength then
                                    visitorInfo.PostError range (error "Record" RecordLength (length range))
                            | SynTypeDefnSimpleRepr.Enum(_) -> 
                                if length range > EnumLength then
                                    visitorInfo.PostError range (error "Enum" EnumLength (length range))
                            | SynTypeDefnSimpleRepr.Union(_) -> 
                                if length range > UnionLength then
                                    visitorInfo.PostError range (error "Union" UnionLength (length range))
                            | _ -> ()
                    | SynTypeDefnRepr.ObjectModel(_) -> 
                        if length range > ClassLength then
                            visitorInfo.PostError range (error "Classes and interface" ClassLength (length range))

                Continue
            | _ -> Continue