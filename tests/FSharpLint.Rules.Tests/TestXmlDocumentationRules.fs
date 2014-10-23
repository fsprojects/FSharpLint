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

module TestXmlDocumentationRules

open NUnit.Framework
open FSharpLint.Rules.XmlDocumentation
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.LoadVisitors

let config = 
    Map.ofList 
        [ 
            (AnalyserName, 
                { 
                    Rules = Map.ofList 
                        [ 
                            ("ExceptionDefinitionHeader", 
                                { 
                                    Settings = Map.ofList 
                                        [ 
                                            ("Enabled", Enabled(true)) 
                                        ] 
                                }) 
                        ]
                    Settings = Map.ofList []
                })
            ]

[<TestFixture>]
type TestNameConventionRules() =
    inherit TestRuleBase.TestRuleBase(Ast(visitor), config)

    [<Test>]
    member this.ExceptionWithDoubleDashComment() = 
        this.Parse """
module Program

// Some exception.
exception SomeException of string"""

        Assert.IsTrue(this.ErrorExistsAt(5, 0))

    [<Test>]
    member this.ExceptionWithDoubleDashCommentSuppressed() = 
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("FSharpLint.XmlDocumentation", "ExceptionDefinitionHeader")>]
module Program

// Some exception.
exception SomeException of string"""

        Assert.IsFalse(this.ErrorExistsOnLine(6))

    [<Test>]
    member this.ExceptionWithMultilineComment() = 
        this.Parse """
module Program

(* Some exception. *)
exception SomeException of string"""

        Assert.IsTrue(this.ErrorExistsAt(5, 0))

    [<Test>]
    member this.ExceptionWithXmlComment() = 
        this.Parse """
module Program

/// Some exception.
exception SomeException of string"""

        Assert.IsFalse(this.ErrorExistsAt(5, 0))

    [<Test>]
    member this.ExceptionNoComment() = 
        this.Parse """
module Program

exception SomeException of string"""

        Assert.IsTrue(this.ErrorExistsAt(4, 0))