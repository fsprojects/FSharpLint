module FSharpLint.Core.Tests.Rules.Smells.NoAsyncRunSynchronouslyInLibrary

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules
open FSharpLint.Rules.NoAsyncRunSynchronouslyInLibrary

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

    [<Test>]
    member this.``Async.RunSynchronously may be used in code module that has function with entry point``() =
        this.Parse("""
module Program

let foo () =
    async {
        return ()
    }
    |> Async.RunSynchronously

[<EntryPoint>]
let main () =
    0""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in NUnit test code``() =
        this.Parse("""
module Program

[<TestFixture>]
type FooTest () =
    [<Test>]
    member this.Foo() =
        async {
            return ()
        }
        |> Async.RunSynchronously""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in MSTest test code``() =
        this.Parse("""
module Program

[<TestClass>]
type FooTest () =
    [<TestMethod>]
    member this.Foo() =
        async {
            return ()
        }
        |> Async.RunSynchronously""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in module with tests``() =
        this.Parse("""
module Program

let foo () =
    async {
        return ()
    }
    |> Async.RunSynchronously

[<TestClass>]
type FooTest () =
    [<TestMethod>]
    member this.Foo() =
        ()""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in methods with Obsolete attribute``() =
        this.Parse("""
module Program

type FooTest () =
    [<Obsolete>]
    member this.Foo() =
        async {
            return ()
        }
        |> Async.RunSynchronously""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in functions with Obsolete attribute``() =
        this.Parse("""
module Program

[<Obsolete>]
let Foo() =
    async {
        return ()
    }
    |> Async.RunSynchronously""")

        this.AssertNoWarnings()

[<TestFixture>]
type TestNoAsyncRunSynchronouslyInLibraryHeuristic() =
    [<Test>]
    member this.``Unlikely to be library if contains "test" in name``() =
        Assert.AreEqual(
            howLikelyProjectIsLibrary "TestProject",
            LibraryHeuristicResultByProjectName.Unlikely
        )

    [<Test>]
    member this.``Unlikely to be library if contains "console" in name``() =
        Assert.AreEqual(
            howLikelyProjectIsLibrary "FooConsole",
            LibraryHeuristicResultByProjectName.Unlikely
        )

    [<Test>]
    member this.``Likely to be library if contains Contains "Lib" as a PascalCase segment``() =
        Assert.AreEqual(
            howLikelyProjectIsLibrary "LibFoo",
            LibraryHeuristicResultByProjectName.Likely
        )

    [<Test>]
    member this.``Uncertain if contains contains "Lib" but not as a PascalCase segment``() =
        Assert.AreEqual(
            howLikelyProjectIsLibrary "LibreOfficeProg",
            LibraryHeuristicResultByProjectName.Uncertain
        )

    [<Test>]
    member this.``Likely to be library if contains ends with "lib" (case-insensitive)``() =
        Assert.AreEqual(
            howLikelyProjectIsLibrary "FooLib",
            LibraryHeuristicResultByProjectName.Likely
        )
