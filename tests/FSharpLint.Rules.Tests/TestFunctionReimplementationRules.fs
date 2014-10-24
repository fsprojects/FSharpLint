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
    Map.ofList 
        [ 
            (AnalyserName, 
                { 
                    Rules = Map.ofList []
                    Settings = Map.ofList 
                        [ 
                            ("Enabled", Enabled(true))
                        ]
                })
            ]

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
    member this.LambdaReimplementingMultiplcationIssuesErrorSuppressed() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("FSharpLint.FunctionReimplementation", "*")>]
let f = fun a b -> a * b
"""

        Assert.IsFalse(this.ErrorExistsOnLine(5))

    [<Test>]
    member this.LambdaNotReimplmentingMultiplicationAsUsingConstantDoesNotIssueError() = 
        this.Parse """
module Program

let f = fun a b -> a * 1
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.LambdaReimplementingCeilingFunctionIssuesError() = 
        this.Parse """
module Program

let f = fun x -> ceil x
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 8))

    [<Test>]
    member this.LambdaWithUnitParameterDoesNotIssueError() = 
        this.Parse """
module Program

let x = 6

let f = fun () -> ceil x
"""

        Assert.IsFalse(this.ErrorExistsAt(6, 8))

    [<Test>]
    member this.LambdaWithWildcardParameterDoesNotIssueError() = 
        this.Parse """
module Program

let x = 6

let f = fun _ -> ceil x
"""

        Assert.IsFalse(this.ErrorExistsAt(6, 8))

    [<Test>]
    member this.MultiplcationLambdaWithWildcardParameterDoesNotIssueError() = 
        this.Parse """
module Program

let x = 6

let f = fun a b _ -> a * b
"""

        Assert.IsFalse(this.ErrorExistsAt(6, 8))