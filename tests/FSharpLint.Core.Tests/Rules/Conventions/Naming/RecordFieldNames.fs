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

type Record = { Dog: int }
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.RecordFieldIsCamelCase() =
        this.Parse """
module Program

type Record = { dog: int }
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 16))

    [<Test>]
    member this.RecordFieldRuleDoesntApplyToUnionCaseFields() =
        this.Parse """
type Test =
| InfixOperator of operatorIdentifier:Expression * Expression * Expression
"""

        Assert.IsTrue(this.NoErrorsExist)
