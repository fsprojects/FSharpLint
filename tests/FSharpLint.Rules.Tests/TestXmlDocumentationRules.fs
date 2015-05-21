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

/// set all XmlDocumentation rules to be disabled, except the one under test
let config name =
    Map.ofList
        [
            (AnalyserName,
                {
                    Rules = ["ModuleDefinitionHeader";
                             "ExceptionDefinitionHeader"
                             "TypeDefinitionHeader";
                             "MemberDefinitionHeader";
                             "EnumDefinitionHeader";
                             "UnionDefinitionHeader";
                             "RecordDefinitionHeader";
                             "AutoPropertyDefinitionHeader"]
                             |> List.map (fun ruleName ->
                                 (ruleName,
                                    {Rule.Settings = Map.ofList
                                            [
                                                ("Enabled", Enabled(ruleName = name))
                                            ]
                                    }))
                             |> Map.ofList
                    Settings = Map.ofList []
                })
            ]

[<TestFixture>]
type TestNameConventionRulesModule() =
    inherit TestRuleBase.TestRuleBase(Ast(visitor), config "ModuleDefinitionHeader")

    [<Test>]
    member this.ModuleWithDoubleDashComment() =
        this.Parse """
// Some module.
module Program

exception SomeException of string"""

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(3, 0), this.ErrorMsg)

    [<Test>]
    member this.ModuleWithDoubleDashCommentSuppressed() =
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("XmlDocumentation", "ModuleDefinitionHeader")>]
// Some module.
module Program

exception SomeException of string"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.ModuleWithMultilineComment() =
        this.Parse """
(* Some module. *)
module Program

exception SomeException of string"""

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(3, 0), this.ErrorMsg)

    [<Test>]
    member this.ModuleWithXmlComment() =
        this.Parse """
/// Some module.
module Program

exception SomeException of string"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.ModuleNoComment() =
        this.Parse """
module Program

exception SomeException of string"""

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(2, 0), this.ErrorMsg)

    [<Test>]
    member this.ModuleWithEmptyXmlComment() =
        this.Parse """
///
module Program

exception SomeException of string"""

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(3, 0), this.ErrorMsg)

[<TestFixture>]
type TestNameConventionRulesException() =
    inherit TestRuleBase.TestRuleBase(Ast(visitor), config "ExceptionDefinitionHeader")

    [<Test>]
    member this.ExceptionWithDoubleDashComment() =
        this.Parse """
module Program

// Some exception.
exception SomeException of string"""

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(5, 0))

    [<Test>]
    member this.ExceptionWithDoubleDashCommentSuppressed() =
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("XmlDocumentation", "ExceptionDefinitionHeader")>]
module Program

// Some exception.
exception SomeException of string"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.ExceptionWithMultilineComment() =
        this.Parse """
module Program

(* Some exception. *)
exception SomeException of string"""

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(5, 0))

    [<Test>]
    member this.ExceptionWithXmlComment() =
        this.Parse """
module Program

/// Some exception.
exception SomeException of string"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.ExceptionNoComment() =
        this.Parse """
module Program

exception SomeException of string"""

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.ExceptionWithEmptyXmlComment() =
        this.Parse """
module Program
///
exception SomeException of string"""

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(4, 0))

[<TestFixture>]
type TestNameConventionRulesType() =
    inherit TestRuleBase.TestRuleBase(Ast(visitor), config "TypeDefinitionHeader")

    [<Test>]
    member this.TypeNoComment() =
        this.Parse """

type IsAType =
    /// this is a member
    member this.Test = true
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(3, 5))

    [<Test>]
    member this.TypeWithDoubleDashComment() =
        this.Parse """
// this is a type
type IsAType =
    /// this is a member
    member this.Test = true
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(3, 5))

    [<Test>]
    member this.TypeWithDoubleDashCommentSuppressed() =
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("XmlDocumentation", "TypeDefinitionHeader")>]
// this is a type
type IsAType =
    /// this is a member
    member this.Test = true
        """

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.TypeWithWhitespaceXmlComment() =
        this.Parse ("""
/// """ + "\t" + """
type IsAType =
    /// this is a member
    member this.Test = true
        """)

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(3, 5))

    [<Test>]
    member this.TypeWithMultilineComment() =
        this.Parse """
(* This is a type *)
type IsAType =
    /// this is a member
    member this.Test = true
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(3, 5))

    [<Test>]
    member this.TypeWithXmlComment() =
        this.Parse """
/// This is a type
///
/// third line is still ok
type IsAType =
    /// this is a member
    member this.Test = true
        """

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.EnumTypeWithXmlComment() =
        this.Parse """
/// This is an enum type
type Colors = Red=0 | Green=1 | Blue=2
        """

        Assert.IsTrue(this.NoErrorsExist)

[<TestFixture>]
type TestNameConventionRulesMember() =
    inherit TestRuleBase.TestRuleBase(Ast(visitor), config "MemberDefinitionHeader")

    [<Test>]
    member this.MemberNoComment() =
        this.Parse """
/// this is a type
type IsAType =

    member this.Test = true
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(5, 11), this.ErrorMsg)

    [<Test>]
    member this.MemberWithDoubleDashComment() =
        this.Parse """
/// this is a type
type IsAType =
    // this is a member
    member this.Test = true
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(5, 11))

    [<Test>]
    member this.MemberWithDoubleDashCommentSuppressed() =
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("XmlDocumentation", "MemberDefinitionHeader")>]
/// this is a type
type IsAType =
    // this is a member
    member this.Test = true
        """

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.MemberWithWhitespaceXmlComment() =
        this.Parse ("""
/// This is a type
type IsAType =
    /// """ + "\t" + """
    member this.Test = true
        """)

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(5, 11))

    [<Test>]
    member this.MemberWithMultilineComment() =
        this.Parse """
/// This is a type
type IsAType =
    (* This is a type *)
    member this.Test = true
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(5, 11))

    [<Test>]
    member this.MemberWithXmlComment() =
        this.Parse """
/// This is a type
type IsAType =
    /// This is a member
    ///
    /// third line is still ok
    member this.Test = true
        """

        Assert.IsTrue(this.NoErrorsExist)

[<TestFixture>]
type TestNameConventionRulesEnum() =
    inherit TestRuleBase.TestRuleBase(Ast(visitor), config "EnumDefinitionHeader")

    [<Test>]
    member this.EnumNoComment() =
        this.Parse """
type Colors =
    | Red=0
    | Green=1
    | Blue=2
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(3), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(5), this.ErrorMsg)

    [<Test>]
    member this.EnumWithDoubleDashComment() =
        this.Parse """
type Colors =
    // This is red
    | Red=0
    // This is green
    | Green=1
    // This is blue
    | Blue=2
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(8), this.ErrorMsg)

    [<Test>]
    member this.EnumWithDoubleDashCommentSuppressed() =
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("XmlDocumentation", "EnumDefinitionHeader")>]
type Colors =
    // This is red
    | Red=0
    // This is green
    | Green=1
    // This is blue
    | Blue=2
        """

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.EnumWithEmptyXmlComment() =
        this.Parse ("""
type Colors =
    ///
    | Red=0
    ///
    | Green=1
    ///
    | Blue=2
        """)

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(8), this.ErrorMsg)

    [<Test>]
    member this.EnumWithMultilineComment() =
        this.Parse """
type Colors =
    (* This is red *)
    | Red=0
    (* This is green *)
    | Green=1
    (* This is blue *)
    | Blue=2
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(8), this.ErrorMsg)

    [<Test>]
    member this.EnumWithXmlComment() =
        this.Parse """
type Colors =
    /// This is red
    | Red=0
    /// This is green
    | Green=1
    /// This is blue
    | Blue=2
        """

        Assert.IsTrue(this.NoErrorsExist, this.ErrorMsg)

    [<Test>]
    member this.EnumWithAMissingXmlComment() =
        this.Parse """
type Colors =
    /// This is red
    | Red=0
    /// This is green
    | Green=1
    // This is blue
    | Blue=2
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(8), this.ErrorMsg)

[<TestFixture>]
type TestNameConventionRulesUnion() =
    inherit TestRuleBase.TestRuleBase(Ast(visitor), config "UnionDefinitionHeader")

    [<Test>]
    member this.UnionNoComment() =
        this.Parse """
type Shape =
    | Rectangle of width : float * length : float
    | Circle of radius : float
    | Shape
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(3), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(5), this.ErrorMsg)

    [<Test>]
    member this.UnionWithDoubleDashComment() =
        this.Parse """
type Shape =
    // this is a rectangle
    | Rectangle of width : float * length : float
    // this is a circle
    | Circle of radius : float
    // this is this
    | Shape
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(8), this.ErrorMsg)

    [<Test>]
    member this.UnionWithDoubleDashCommentSuppressed() =
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("XmlDocumentation", "UnionDefinitionHeader")>]
type Shape =
    // this is a rectangle
    | Rectangle of width : float * length : float
    // this is a circle
    | Circle of radius : float
    // this is this
    | Shape
        """

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.UnionWithEmptyXmlComment() =
        this.Parse ("""
type Shape =
    ///
    | Rectangle of width : float * length : float
    ///
    | Circle of radius : float
    ///
    | Shape
        """)

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(8), this.ErrorMsg)

    [<Test>]
    member this.UnionWithMultilineComment() =
        this.Parse """
type Shape =
    (* this is a rectangle *)
    | Rectangle of width : float * length : float
    (* this is a circle *)
    | Circle of radius : float
    (* this is this *)
    | Shape
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(8), this.ErrorMsg)

    [<Test>]
    member this.UnionWithXmlComment() =
        this.Parse """
type Shape =
    /// this is a rectangle
    | Rectangle of width : float * length : float
    /// this is a circle
    | Circle of radius : float
    /// this is this
    | Shape
        """

        Assert.IsTrue(this.NoErrorsExist, this.ErrorMsg)

    [<Test>]
    member this.UnionWithAMissingXmlComment() =
        this.Parse """
type Shape =
    /// this is a rectangle
    | Rectangle of width : float * length : float
    // this is a circle
    | Circle of radius : float
    /// this is this
    | Shape
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)

    [<Test>]
    member this.UnionSingleCaseWithXmlComment() =
        this.Parse """
type OrderId =
    /// this is an order id
    | OrderId of int
        """

        Assert.IsTrue(this.NoErrorsExist, this.ErrorMsg)

    [<Test>]
    member this.UnionSingleCaseWithNoXmlComment() =
        this.Parse """
type OrderId =

    | OrderId of int
        """

        Assert.IsTrue(this.ErrorsExist, this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)

[<TestFixture>]
type TestNameConventionRulesRecord() =
    inherit TestRuleBase.TestRuleBase(Ast(visitor), config "RecordDefinitionHeader")

    [<Test>]
    member this.RecordNoComment() =
        this.Parse """
type GeoCoord = {

    lat: float

    long: float
    }
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)

    [<Test>]
    member this.RecordWithDoubleDashComment() =
        this.Parse """
type GeoCoord = {
    // this is latitude
    lat: float
    // this is longitude
    long: float
    }
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)

    [<Test>]
    member this.RecordWithDoubleDashCommentSuppressed() =
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("XmlDocumentation", "RecordDefinitionHeader")>]
type GeoCoord = {
    // this is latitude
    lat: float
    // this is longitude
    long: float
    }
        """

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.RecordWithEmptyXmlComment() =
        this.Parse ("""
type GeoCoord = {
    ///
    lat: float
    ///
    long: float
    }
        """)

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)

    [<Test>]
    member this.RecordWithMultilineComment() =
        this.Parse """
type GeoCoord = {
    (* this is latitude *)
    lat: float
    (* this is longitude *)
    long: float
    }
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)

    [<Test>]
    member this.RecordWithXmlComment() =
        this.Parse """
type GeoCoord = {
    /// this is latitude
    lat: float
    /// this is longitude
    long: float
    }
        """

        Assert.IsTrue(this.NoErrorsExist, this.ErrorMsg)

    [<Test>]
    member this.RecordWithAMissingXmlComment() =
        this.Parse """
type GeoCoord = {
    /// this is latitude
    lat: float
    // this is longitude
    long: float
    }
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)

[<TestFixture>]
type TestNameConventionRulesAutoProperty() =
    inherit TestRuleBase.TestRuleBase(Ast(visitor), config "AutoPropertyDefinitionHeader")

    [<Test>]
    member this.AutoPropertyNoComment() =
        this.Parse """
type GeoCoord() =
    // this is latitude
    member val Lat = 0 with get, set
    // this is longitude
    member val Long = 0 with get, set
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)

    [<Test>]
    member this.AutoPropertyWithDoubleDashComment() =
        this.Parse """
type GeoCoord() =
    // this is latitude
    member val Lat = 0 with get, set
    // this is longitude
    member val Long = 0 with get, set
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)

    [<Test>]
    member this.AutoPropertyWithDoubleDashCommentSuppressed() =
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("XmlDocumentation", "AutoPropertyDefinitionHeader")>]
type GeoCoord() =
    // this is latitude
    member val Lat = 0 with get, set
    // this is longitude
    member val Long = 0 with get, set
        """

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.AutoPropertyWithEmptyXmlComment() =
        this.Parse ("""
type GeoCoord() =
    ///
    member val Lat = 0 with get, set
    ///
    member val Long = 0 with get, set
        """)

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)

    [<Test>]
    member this.AutoPropertyWithMultilineComment() =
        this.Parse """
type GeoCoord() =
    (* this is latitude *)
    member val Lat = 0 with get, set
    (* this is longitude *)
    member val Long = 0 with get, set
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(4), this.ErrorMsg)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)

    [<Test>]
    member this.AutoPropertyWithXmlComment() =
        this.Parse """
type GeoCoord() =
    /// this is latitude
    member val Lat = 0 with get, set
    /// this is longitude
    member val Long = 0 with get, set
        """

        Assert.IsTrue(this.NoErrorsExist, this.ErrorMsg)

    [<Test>]
    member this.AutoPropertyWithAMissingXmlComment() =
        this.Parse """
type GeoCoord() =
    /// this is latitude
    member val Lat = 0 with get, set
    // this is longitude
    member val Long = 0 with get, set
        """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine(6), this.ErrorMsg)
