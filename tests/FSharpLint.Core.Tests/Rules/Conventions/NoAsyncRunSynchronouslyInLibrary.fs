module FSharpLint.Core.Tests.Rules.Conventions.NoAsyncRunSynchronouslyInLibrary

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<TestFixture>]
type TestNoAsyncRunSynchronouslyInLibrary() =
    inherit FSharpLint.Core.Tests.TestAstNodeRuleBase.TestAstNodeRuleBase(NoAsyncRunSynchronouslyInLibrary.rule)

    [<Test>]
    member this.``Async.RunSynchronously should not be used in library code``() =
        this.Parse("""
module Program

async {
    return ()
}
|> Async.RunSynchronously""")

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Async.RunSynchronously may be used in code that declares entry point``() =
        this.Parse("""
module Program

[<EntryPoint>]
let main () =
    async {
        return ()
    }
    |> Async.RunSynchronously""")

        this.AssertNoWarnings()
