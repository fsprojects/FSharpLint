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

module TestFunctionReimplementationRules

open NUnit.Framework
open FSharpLint.Rules.FunctionReimplementation
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.LoadVisitors

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
    inherit TestRuleBase.TestRuleBase(Ast(visitor), config)

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

        Assert.IsFalse(this.ErrorsExist)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/113
    [<Test>]
    member this.``Lambda to pointfree long identifer constructor application should not be suggested unless using F# 4 or above``() = 
        this.Parse("""
module Program

let f = List.map (fun x -> System.Uri x) ["1";"2"]
""", checkInput = true, fsharpVersion = System.Version(3, 1))

        Assert.IsFalse(this.ErrorsExist)

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

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaReimplementingMultiplcationIssuesErrorSuppressedWithRuleName() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("FunctionReimplementation", "ReimplementsFunction")>]
let f = fun a b -> a * b
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaNotReimplmentingMultiplicationAsUsingConstantDoesNotIssueError() = 
        this.Parse """
module Program

let f = fun a b -> a * 1
"""

        Assert.IsFalse(this.ErrorsExist)

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

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError() = 
        this.Parse """
module Program

let f = fun x -> x |> tan |> cos |> tan
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError2() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> tan y x |> cos y |> tan (tan y)
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 8))

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError3() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> tan y x |> cos x |> tan y
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError4() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> tan x x |> cos y |> tan y
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError5() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> tan y x |> cos (fun _ -> x) |> tan y
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError6() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> tan y x |> cos (fun x -> x) |> tan y
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 8))

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError7() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> tan y x |> cos (fun _ -> y) |> tan y
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 8))

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError8() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> 
    tan y x 
        |> cos (fun _ -> 
                        let x = 7
                        x) |> tan y
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 8))

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError9() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> 
    tan y x 
        |> cos (fun _ -> 
                        let y = x
                        let x = 7
                        y) |> tan y
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError10() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> 
    tan y x 
        |> cos (fun _ -> 
                        let y = 7
                        x) |> tan y
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError11() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> 
    tan y x 
        |> cos (function | y -> x) |> tan y
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError12() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> 
    tan y x 
        |> cos (function | x -> x) |> tan y
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 8))

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError13() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> 
    tan y x 
        |> cos (fun _ -> match x with | _ -> 0) |> tan y
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError14() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> 
    tan y x 
        |> cos (fun _ -> match y with | _ -> x) |> tan y
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError15() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> 
    tan y x 
        |> cos (fun _ -> match y with | x -> x) |> tan y
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 8))

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError16() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> 
    tan y x 
        |> cos (fun _ -> for y = 0 to 10 do ignore x) |> tan y
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError17() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> 
    tan y x 
        |> cos (fun _ -> for x = 0 to 10 do ignore x) |> tan y
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 8))

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesError18() = 
        this.Parse """
module Program

let y = 0
let f = fun x -> 
    tan y x 
        |> cos (fun _ -> for y = 0 to x do ()) |> tan y
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaWithUnitParameterDoesNotIssueError() = 
        this.Parse """
module Program

let x = 6

let f = fun () -> ceil x
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LambdaWithWildcardParameterDoesNotIssueError() = 
        this.Parse """
module Program

let x = 6

let f = fun _ -> ceil x
"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.MultiplcationLambdaWithWildcardParameterDoesNotIssueError() = 
        this.Parse """
module Program

let x = 6

let f = fun a b _ -> a * b
"""

        Assert.IsFalse(this.ErrorsExist)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/130
    [<Test>]
    member this.``No suggestion should be given for function composition when the lambda's parameter's property/field is accessed``() = 
        this.Parse """
module Program

let x = 6

let f = fun p -> p.Name <= packageName || not (isPackageLastInSource p)
"""

        Assert.IsFalse(this.ErrorsExist)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/140
    [<Test>]
    member this.``No suggestion should be given for function composition when lambda has multiple arguments``() = 
        this.Parse """
module Program

let x = 6

let f = fun s1 s2 -> concat s1 s2 |> parse
"""

        Assert.IsFalse(this.ErrorsExist)