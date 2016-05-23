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

module TestRaiseWithTooManyArgumentsRules

open NUnit.Framework
open FSharpLint.Rules.RaiseWithTooManyArguments
open FSharpLint.Framework.Configuration

let config = 
    let ruleEnabled = { Rule.Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }

    Map.ofList 
        [ (AnalyserName, 
            { Rules = Map.ofList 
                [ ("FailwithWithSingleArgument", ruleEnabled) 
                  ("RaiseWithSingleArgument", ruleEnabled) 
                  ("NullArgWithSingleArgument", ruleEnabled) 
                  ("InvalidOpWithSingleArgument", ruleEnabled) 
                  ("InvalidArgWithTwoArguments", ruleEnabled) 
                  ("FailwithfWithArgumentsMatchingFormatString", ruleEnabled) ]
              Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }) ]
               
[<TestFixture>]
type TestRaiseWithTooManyArgumentsRules() =
    inherit TestRuleBase.TestRuleBase(visitor, config)

    [<Category("Performance")>]
    [<Test>]
    member this.``Performance of raise with too many args analyser``() = 
        Assert.Less(this.TimeAnalyser(100, defaultConfiguration), 20)

    [<Test>]
    member this.FailwithWithCorrectNumberOfArguments() = 
        this.Parse """
module Program

failwith "" """

        Assert.IsFalse(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.FailwithWithExtraArgument() = 
        this.Parse """
module Program

failwith "" "" """

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.FailwithWithExtraArgumentSuppressed() = 
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("RaiseWithTooManyArguments", "FailwithWithSingleArgument")>]
module Program

failwith "" "" """

        Assert.IsFalse(this.ErrorExistsOnLine(5))

    [<Test>]
    member this.FailwithWithMultipleExtraArguments() = 
        this.Parse """
module Program

failwith "" "" "" "" """

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.FailwithWithExtraArgumentWithRightPipe() = 
        this.Parse """
module Program

"" |> failwith "" """

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.RaiseWithCorrectArguments() = 
        this.Parse """
module Program

raise (System.ArgumentException("Divisor cannot be zero!")) """

        Assert.IsFalse(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.RaiseWithExtraArgument() = 
        this.Parse """
module Program

raise (System.ArgumentException("Divisor cannot be zero!")) "" """

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.RaiseWithExtraArgumentSuppressed() = 
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("RaiseWithTooManyArguments", "RaiseWithSingleArgument")>]
module Program

raise (System.ArgumentException("Divisor cannot be zero!")) "" """

        Assert.IsFalse(this.ErrorExistsOnLine(5))

    [<Test>]
    member this.FailwithfWithCorrectNumberOfArguments() = 
        this.Parse """
module Program

failwithf "%d %s" 4 "dog" """

        Assert.IsFalse(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.FailwithfWithExtraArgument() = 
        this.Parse """
module Program

failwithf "%d %s" 4 "dog" 5 """

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.FailwithfWithExtraArgumentSuppressed() = 
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("RaiseWithTooManyArguments", "FailwithfWithArgumentsMatchingFormatString")>]
module Program

failwithf "%d %s" 4 "dog" 5 """

        Assert.IsFalse(this.ErrorExistsOnLine(5))

    [<Test>]
    member this.FailwithfWithEscapedFormatAndWithExtraArgument() = 
        this.Parse """
module Program

failwithf "%d %% %s" 4 "dog" 5 """

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.NullArgWithCorrectNumberOfArguments() = 
        this.Parse """
module Program

nullArg "" """

        Assert.IsFalse(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.NullArgWithExtraArgument() = 
        this.Parse """
module Program

nullArg "" "" """

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.NullArgWithExtraArgumentSuppressed() = 
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("RaiseWithTooManyArguments", "NullArgWithSingleArgument")>]
module Program

nullArg "" "" """

        Assert.IsFalse(this.ErrorExistsOnLine(5))

    [<Test>]
    member this.InvalidOpWithCorrectNumberOfArguments() = 
        this.Parse """
module Program

invalidOp "" """

        Assert.IsFalse(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.InvalidOpWithExtraArgument() = 
        this.Parse """
module Program

invalidOp "" "" """

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.InvalidOpWithExtraArgumentSuppressed() = 
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("RaiseWithTooManyArguments", "InvalidOpWithSingleArgument")>]
module Program

invalidOp "" "" """

        Assert.IsFalse(this.ErrorExistsOnLine(5))

    [<Test>]
    member this.InvalidArgWithCorrectNumberOfArguments() = 
        this.Parse """
module Program

invalidArg "month" "Expected value to be between 1 and 12" """

        Assert.IsFalse(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.InvalidArgWithExtraArgument() = 
        this.Parse """
module Program

invalidArg "month" "Expected value to be between 1 and 12" "some other arg" """

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.InvalidArgWithExtraArgumentSuppressed() = 
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("RaiseWithTooManyArguments", "InvalidArgWithTwoArguments")>]
module Program

invalidArg "month" "Expected value to be between 1 and 12" "some other arg" """

        Assert.IsFalse(this.ErrorExistsOnLine(5))