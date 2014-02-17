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

module AstVisitorBase =

    open System
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices

    type Continue =
    | Continue
    | End

        member this.ShallContinue = 
            match this with
            | Continue -> true
            | End -> false

    [<AbstractClass>]
    type AstVisitorBase(checkFile:CheckFileResults) =
        abstract VisitModuleOrNamespace : 
            LongIdent * bool * SynModuleDecls * PreXmlDoc * SynAttributes * SynAccess option * range -> Continue
        default this.VisitModuleOrNamespace(_, _, _, _, _, _, _) = Continue

        abstract VisitUnionCase : 
            SynAttributes * Ident * SynUnionCaseType * PreXmlDoc * SynAccess option * range -> Continue
        default this.VisitUnionCase(_, _, _, _, _, _) = Continue

        abstract VisitEnumCase : 
            SynAttributes * Ident * SynConst * PreXmlDoc * range -> Continue
        default this.VisitEnumCase(_, _, _, _, _) = Continue

        abstract VisitField : 
            SynAttributes * Ident option * SynType * PreXmlDoc * SynAccess option * range -> Continue
        default this.VisitField(_, _, _, _, _, _) = Continue

        abstract VisitComponentInfo : 
            SynAttributes * SynTyparDecl list * SynTypeConstraint list * LongIdent * PreXmlDoc * SynAccess option * range -> Continue
        default this.VisitComponentInfo(_, _, _, _, _, _, _) = Continue

        abstract VisitExceptionRepresentation : 
            SynAttributes * SynUnionCase * LongIdent option * PreXmlDoc * SynAccess option * range -> Continue
        default this.VisitExceptionRepresentation(_, _, _, _, _, _) = Continue

        abstract VisitNamedPattern : 
            SynPat * Ident * bool * SynAccess option * range -> Continue
        default this.VisitNamedPattern(_, _, _, _, _) = Continue

        abstract VisitIdPattern : 
            Ident * range -> Continue
        default this.VisitIdPattern(_, _) = Continue
        
        abstract VisitLongIdentPattern :
            LongIdentWithDots * Ident option * SynAccess option * range -> Continue
        default this.VisitLongIdentPattern(_, _, _, _) = Continue