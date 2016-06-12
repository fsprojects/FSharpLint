// FSharpLint, a linter for F#.
// Copyright (C) 2016 Matthew Mcveigh
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

module TestFunctionReimplementationRules

open NUnit.Framework
open FSharpLint.Rules.FunctionReimplementation
open FSharpLint.Framework.Configuration

let config = 
    let ruleEnabled = { Rule.Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }

    Map.ofList 
        [ (AnalyserName, 
            { Rules = Map.ofList 
                [ ("CanBeReplacedWithComposition", ruleEnabled) 
                  ("ReimplementsFunction", ruleEnabled) ]
              Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }) ]
 
[<TestFixture>]
type TestFunctionReimplementationRules() =
    inherit TestRuleBase.TestRuleBase(analyser, config)

    [<Category("Performance")>]
    [<Test>]
    member this.``Performance of function reimplementation analyser``() = 
        Assert.Less(this.TimeAnalyser(100, defaultConfiguration), 20)

    [<Test>]
    member this.LambdaReimplementingMultiplcationIssuesError() = 
        this.Parse """
module Program

let f = fun a b -> a * b
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.``Operator ident is displayed in error as an operator symbol.``() = 
        this.Parse """
module Program

let f = fun a b -> a * b
"""

        this.ErrorMsg.Contains "`( * )`" |> Assert.IsTrue

    [<Test>]
    member this.``Lambda reimplementing long identifier function issues error``() = 
        this.Parse """
module Program

let f = fun a b -> List.map a b
"""

        Assert.IsTrue(this.ErrorsExist)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/113
    [<Test>]
    member this.``Lambda to pointfree constructor application should not be suggested unless using F# 4 or above``() = 
        this.Parse("""
module Program

type Duck(info:string) =
    do ()

let f = List.map (fun x -> Duck x) ["1";"2"]

open System
let f = List.map (fun x -> Uri x) ["1";"2"]
""", checkInput = true, fsharpVersion = System.Version(3, 1))

        this.AssertNoWarnings()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/113
    [<Test>]
    member this.``Lambda to pointfree long identifer constructor application should not be suggested unless using F# 4 or above``() = 
        this.Parse("""
module Program

let f = List.map (fun x -> System.Uri x) ["1";"2"]
""", checkInput = true, fsharpVersion = System.Version(3, 1))

        this.AssertNoWarnings()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/113
    [<Test>]
    member this.``Lambda to DU constructor application should be suggested when using any version of F#``() = 
        this.Parse("""
module Program

type Cat = | Meower of string

let f = List.map (fun x -> Meower x) ["1";"2"]
""", checkInput = true, fsharpVersion = System.Version(3, 1))

        Assert.IsTrue(this.ErrorsExist)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/113
    [<Test>]
    member this.``Lambda to pointfree constructor application should be suggested if using F# 4 or above``() = 
        this.Parse("""
module Program

type Duck(info:string) =
    do ()

let f = List.map (fun x -> Duck x) ["1";"2"]

open System
let f = List.map (fun x -> String x) ["1";"2"]
""", checkInput = true, fsharpVersion = System.Version(4, 0))

        Assert.IsTrue(this.ErrorsExist)

    [<Test>]
    member this.LambdaReimplementingMultiplcationIssuesErrorSuppressed() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("FunctionReimplementation", "*")>]
let f = fun a b -> a * b
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.LambdaReimplementingMultiplcationIssuesErrorSuppressedWithRuleName() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("FunctionReimplementation", "ReimplementsFunction")>]
let f = fun a b -> a * b
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.LambdaNotReimplmentingMultiplicationAsUsingConstantDoesNotIssueError() = 
        this.Parse """
module Program

let f = fun a b -> a * 1
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.LambdaReimplementingCeilingFunctionIssuesError() = 
        this.Parse """
module Program

let f = fun x -> ceil x
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.LambdaNestedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError() = 
        this.Parse """
module Program

let f = fun x -> tan(cos(tan x))
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.LambdaReplacableWithCompositionErrorSuppressedWithRuleName() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("FunctionReimplementation", "CanBeReplacedWithComposition")>]
let f = fun x -> tan(cos(tan x))
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError() = 
        this.Parse """
module Program

let f = fun x -> x |> tan |> cos |> tan
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.``Applying constants in chain issues composition suggestion``() = 
        this.Parse """
module Program

let f = fun x -> x |> tan 0 |> cos |> tan
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.``Applying closed over identifier in chain will not issue composition suggestion``() = 
        this.Parse """
module Program

let f = fun x -> x |> tan y |> cos |> tan
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.LambdaWithUnitParameterDoesNotIssueError() = 
        this.Parse """
module Program

let x = 6

let f = fun () -> ceil x
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.LambdaWithWildcardParameterDoesNotIssueError() = 
        this.Parse """
module Program

let x = 6

let f = fun _ -> ceil x
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.MultiplcationLambdaWithWildcardParameterDoesNotIssueError() = 
        this.Parse """
module Program

let x = 6

let f = fun a b _ -> a * b
"""

        this.AssertNoWarnings()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/130
    [<Test>]
    member this.``No suggestion should be given for function composition when the lambda's parameter's property/field is accessed``() = 
        this.Parse """
module Program

let x = 6

let f = fun p -> p.Name <= packageName || not (isPackageLastInSource p)
"""

        this.AssertNoWarnings()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/140
    [<Test>]
    member this.``No suggestion should be given for function composition when an infix operator is in the expression``() = 
        this.Parse """
module Program

let x = 6

let f = (fun value -> state + (findCoefficient conversion.Coefficients key) * value) 
"""

        this.AssertNoWarnings()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/140
    [<Test>]
    member this.``No suggestion should be given for function composition when lambda has multiple arguments``() = 
        this.Parse """
module Program

let x = 6

let f = fun s1 s2 -> concat s1 s2 |> parse
"""

        this.AssertNoWarnings()