module FSharpLint.Core.Tests.Rules.Formatting.PatternMatchClauseIndentation

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestFormattingPatternMatchClauseIndentation() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(PatternMatchClauseIndentation.rule { PatternMatchClauseIndentation.Config.AllowSingleLineLambda = false })

    [<Test>]
    member this.``Violation for pattern match clauses at different indentation``() =
        this.Parse"""
module Program

match 1 with
| 1 -> true
    | 2 -> false
"""

        Assert.IsTrue(this.ViolationExistsAt(6, 6))

    [<Test>]
    member this.``No violation for pattern match clauses with same indentation``() =
        this.Parse"""
module Program

match 1 with
| 1 -> true
| 2 -> false
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``Violation for exception pattern match clauses at different indentation``() =
        this.Parse """
module Program

try
    2/0
with
    | :? System.DivideByZeroException -> 1
    | :? System.Exception -> 2 """

        Assert.IsTrue(this.ViolationExistsAt(7, 6))

    [<Test>]
    member this.``No violation for exception pattern match clauses with same indentation``() =
        this.Parse """
module Program

try
    2/0
with
| :? System.DivideByZeroException -> 1
| :? System.Exception -> 2 """

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``Violation for lambda pattern match clauses at different indentation``() =
        this.Parse"""
module Program

1 |> (function
    | 1 -> true
        | 2 -> false)
"""

        Assert.IsTrue(this.ViolationExistsAt(6, 10))

    [<Test>]
    member this.``No violation for lambda pattern match clauses with same indentation``() =
        this.Parse"""
module Program

1 |> (function
    | 1 -> true
    | 2 -> false)
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for lambda pattern match clauses with no surrounding parentheses, same indentation``() =
        this.Parse"""
module Program

1 |> function
    | 1 -> true
    | 2 -> false
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``Violation for lambda pattern match clauses without level of indentation for clauses``() =
        this.Parse"""
module Program

1 |> (function
| 1 -> true
| 2 -> false)
"""

        Assert.IsTrue(this.ViolationExistsAt(5, 2))

    [<Test>]
    member this.``No violation for multi-line pattern match clauses with same indentation``() =
        this.Parse"""
module Program

match "x" with
| "a" when 5 > 3 &&
           4 < 8 &&
           2 > 9 -> "result"
| _ -> "otherresult"
"""

        Assert.IsTrue(this.NoViolationsExist)


    [<Test>]
    member this.``No match clause indentation violation for struct tuple deconstruction``() =
          this.Parse """fun struct(x, y) -> ()"""

          Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``Violation for single-line lambda pattern match when not allowed``() =
        this.Parse """let isAnyMatch = function ((SyntaxHintNode.Wildcard | SyntaxHintNode.Variable), _, _, _) -> true | _ -> false"""

        Assert.IsTrue(this.ViolationExistsAt(1, 26))

[<TestFixture>]
type TestFormattingPatternMatchClauseIndentationAllowSingleLineLambda() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(PatternMatchClauseIndentation.rule { PatternMatchClauseIndentation.Config.AllowSingleLineLambda = true })

    [<Test>]
    member this.``No violation for single-line lambda pattern match when allowed``() =
        this.Parse """let isAnyMatch = function ((SyntaxHintNode.Wildcard | SyntaxHintNode.Variable), _, _, _) -> true | _ -> false"""

        Assert.IsTrue(this.NoViolationsExist)


