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

module AstVisitorBase =

    open System
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices

    [<AbstractClass>]
    type AstVisitorBase(checkFile:CheckFileResults) =
        abstract VisitModuleOrNamespace : 
            LongIdent * bool * SynModuleDecls * PreXmlDoc * SynAttributes * SynAccess option * range -> AstVisitorBase list
        default this.VisitModuleOrNamespace(_, _, _, _, _, _, _) = [this]

        abstract VisitUnionCase : 
            SynAttributes * Ident * SynUnionCaseType * PreXmlDoc * SynAccess option * range -> AstVisitorBase list
        default this.VisitUnionCase(_, _, _, _, _, _) = [this]

        abstract VisitEnumCase : 
            SynAttributes * Ident * SynConst * PreXmlDoc * range -> AstVisitorBase list
        default this.VisitEnumCase(_, _, _, _, _) = [this]

        abstract VisitField : 
            SynAttributes * Ident option * SynType * PreXmlDoc * SynAccess option * range -> AstVisitorBase list
        default this.VisitField(_, _, _, _, _, _) = [this]

        abstract VisitComponentInfo : 
            SynAttributes * SynTyparDecl list * SynTypeConstraint list * LongIdent * PreXmlDoc * SynAccess option * range -> AstVisitorBase list
        default this.VisitComponentInfo(_, _, _, _, _, _, _) = [this]

        abstract VisitExceptionRepresentation : 
            SynAttributes * SynUnionCase * LongIdent option * PreXmlDoc * SynAccess option * range -> AstVisitorBase list
        default this.VisitExceptionRepresentation(_, _, _, _, _, _) = [this]

        abstract VisitNamedPattern : 
            SynPat * Ident * bool * SynAccess option * range -> AstVisitorBase list
        default this.VisitNamedPattern(_, _, _, _, _) = [this]

        abstract VisitIdPattern : 
            Ident * range -> AstVisitorBase list
        default this.VisitIdPattern(_, _) = [this]
        
        abstract VisitLongIdentPattern :
            LongIdentWithDots * Ident option * SynConstructorArgs * SynAccess option * range -> AstVisitorBase list
        default this.VisitLongIdentPattern(_, _, _, _, _) = [this]

        abstract VisitValueSignature : 
            Ident * range -> AstVisitorBase list
        default this.VisitValueSignature(_, _) = [this]

        abstract VisitFor : 
            Ident * range -> AstVisitorBase list
        default this.VisitFor(_, _) = [this]

        abstract VisitBinding : 
            SynPat * range -> AstVisitorBase list
        default this.VisitBinding(_, _) = [this]