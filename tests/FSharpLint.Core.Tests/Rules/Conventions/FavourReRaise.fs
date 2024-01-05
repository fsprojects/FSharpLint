module FSharpLint.Core.Tests.Rules.Conventions.FavourReRaise

open NUnit.Framework
open FSharpLint.Rules

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
        reraise() """

        this.AssertNoWarnings()

    [<Test>]
    member this.``using raise ex must generate error``() =
        this.Parse
            """
 try
     foo ()
 with
 | ex ->
     if someCondition then
         raise ex """

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(7, 9))

    [<Test>]
    member this.``using raise outside with block must not generate error``() =
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
      printfn "Always print this." """

        this.AssertNoWarnings()

    [<Test>]
    member this.``using raise with an exception that's not in the with block must not generate error``() =
        this.Parse
            """
try
    foo bar
with
| ex ->
    let e = ex.InnerException
    if null <> e then
        raise e """

        this.AssertNoWarnings()

    [<Test>]
    member this.``using raise with an exception that's not in the with block must not generate error (2)``() =
        this.Parse
            """
try
    foo bar
with
| ex ->
    let e = Exception("baz", ex)
    raise e """

        this.AssertNoWarnings()

    [<Test>]
    member this.``using raise with an exception that's not in the with block must not generate error (3)``() =
        this.Parse
            """
try
    foo bar
with
| e ->
    let ex = Exception("baz", e)
    raise ex """

        this.AssertNoWarnings()

    [<Test>]
    member this.``using raise ex must generate error suggested fix``() =
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

        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(7, 8))

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)
