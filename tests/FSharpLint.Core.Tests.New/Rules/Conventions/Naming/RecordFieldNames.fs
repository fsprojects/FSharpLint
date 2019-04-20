module FSharpLint.Core.Tests.Rules.Conventions.RecordFieldNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.naming = Some NamingCase.PascalCase
      underscores = Some NamingUnderscores.None
      prefix = None
      suffix = None }
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
    member this.RecordFieldIsCamelCaseSuppressed() =
        this.Parse """
module Program
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "RecordFieldNames")>]
  type Record = { dog: int }"""

        this.AssertNoWarnings()

