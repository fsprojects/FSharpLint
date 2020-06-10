module FSharpLint.Core.Tests.Rules.Conventions.RecordFieldNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.Naming = Some NamingCase.PascalCase
      Underscores = Some NamingUnderscores.None
      Prefix = None
      Suffix = None }
[<TestFixture>]
type TestConventionsRecordFieldNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(RecordFieldNames.rule config)

    [<Test>]
    member this.RecordFieldIsPascalCase() =
        this.Parse """
module Program
  type Record = { Dog: int }"""

        this.AssertNoWarnings()

    [<Test>]
    member this.RecordFieldIsCamelCase() =
        this.Parse """
module Program
  type Record = { dog: int }"""

        Assert.IsTrue(this.ErrorExistsAt(3, 18))

    [<Test>]
    member this.RecordFieldRuleDoesntApplyToUnionCaseFields() =
        this.Parse """
  match x with
  | Expression (SynExpr.App(funcExpr=(SynExpr.App(isInfix=isInfix; argExpr=innerArg; funcExpr=funcExpr)); argExpr=outerArg)) -> true
  | _ -> false"""

        Assert.IsTrue(this.NoErrorsExist)
