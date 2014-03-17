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

namespace FSharpLint.Framework

module TypeChecking =
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast

    /// Is an identifier a class property?
    let (|Property|NotProperty|) = function
        | Some(identifier:Ident) ->
            match identifier.idText with
            | "set" | "get" -> Property identifier.idRange
            | _ ->  NotProperty
        | _ ->  NotProperty

    /// Possible types a long identifier as part of a pattern may be representing.
    type LongIdentPatternType =
        | Member
        | ActivePatternDefinition
        | ValueOrFunction
        | Other

    let longIdentPatternType (longIdentifier:LongIdentWithDots) (identifier:Ident option) (checkFile:CheckFileResults) = 
        let name = longIdentifier.Lid.[(longIdentifier.Lid.Length - 1)]

        let range = match identifier with
                    | Property(range) -> range
                    | NotProperty -> name.idRange

        let line, endColumn, ident = range.EndLine, range.EndColumn, name.idText

        let symbol = checkFile.GetSymbolAtLocation(line - 1, endColumn, "", [ident])
        
        match symbol with
            | Some(symbol) ->
                match symbol with
                    | :? FSharpMemberFunctionOrValue as memberFunctionOrValue -> 
                        if memberFunctionOrValue.IsMember then
                            Member
                        else if memberFunctionOrValue.IsActivePattern then
                            ActivePatternDefinition
                        else 
                            ValueOrFunction
                    | _ -> Other
            | None -> 
                Other