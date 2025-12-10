module FSharpLint.Core.Tests.Rules.Conventions.FavourReRaise

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestBindingFavourReRaise() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourReRaise.rule)

    [<Test>]
    member this.``favour reraise() instead of raise``() =
        this.Parse
            """
try
    foo ()
with
| ex ->
    if someCondition then
        reraise()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``using raise ex must generate violation``() =
        this.Parse
            """
 try
     foo ()
 with
 | ex ->
     if someCondition then
         raise ex
"""

        Assert.IsTrue(this.ViolationsExist)
        Assert.IsTrue(this.ViolationExistsAt(7, 9))

    [<Test>]
    member this.``using raise outside with block must not generate violation``() =
        this.Parse
            """
let function1 x y =
   try
     try
        if x = y then raise (InnerError("inner"))
        else raise (OuterError("outer"))
     with
      | InnerError(str) -> printfn "Error1 %s" str
   finally
      printfn "Always print this."
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``using raise with an exception that's not in the with block must not generate violation``() =
        this.Parse
            """
try
    foo bar
with
| ex ->
    let e = ex.InnerException
    if null <> e then
        raise e
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``using raise with an exception that's not in the with block must not generate violation (2)``() =
        this.Parse
            """
try
    foo bar
with
| ex ->
    let e = Exception("baz", ex)
    raise e
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``using raise with an exception that's not in the with block must not generate violation (3)``() =
        this.Parse
            """
try
    foo bar
with
| e ->
    let ex = Exception("baz", e)
    raise ex
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``using raise ex must generate violation and suggested fix``() =
        let source = """
try
    foo ()
with
| ex ->
    if someCondition then
        raise ex """
        
        let expected = """
try
    foo ()
with
| ex ->
    if someCondition then
        reraise() """
        
        this.Parse source

        Assert.IsTrue(this.ViolationsExist)
        Assert.IsTrue(this.ViolationExistsAt(7, 8))

        let result = this.ApplyAutoFix source

        Assert.AreEqual(expected, result)
