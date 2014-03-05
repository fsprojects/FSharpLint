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
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast

    let isPreXmlDocEmpty (preXmlDoc:PreXmlDoc) =
        match preXmlDoc.ToXmlDoc() with
        | XmlDoc(lines) when Array.length lines = 0 -> true
        | _ -> false

    let visitor postError (checkFile:CheckFileResults) astNode = 
        match astNode.Node with
            | AstNode.ExceptionRepresentation(SynExceptionRepr.ExceptionDefnRepr(_, unionCase, _, xmlDoc, _, range)) -> 
                match xmlDoc.ToXmlDoc() with
                    | XmlDoc(lines) when Array.length lines = 0 -> 
                        postError range "Expected exception type to have xml documentation."
                    | _ -> ()
            | _ -> ()

        Continue