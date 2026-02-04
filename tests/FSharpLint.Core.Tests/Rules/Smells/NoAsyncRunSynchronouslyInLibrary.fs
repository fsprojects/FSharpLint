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
            LibraryHeuristicResultByProjectName.Unlikely,
            howLikelyProjectIsLibrary "TestProject"
        )

    [<Test>]
    member this.``Unlikely to be library if contains "console" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByProjectName.Unlikely,
            howLikelyProjectIsLibrary "FooConsole"
        )

    [<Test>]
    member this.``Likely to be library if contains Contains "Lib" as a PascalCase segment``() =
        Assert.AreEqual(
            LibraryHeuristicResultByProjectName.Likely,
            howLikelyProjectIsLibrary "LibFoo"
        )

    [<Test>]
    member this.``Uncertain if contains contains "Lib" but not as a PascalCase segment``() =
        Assert.AreEqual(
            LibraryHeuristicResultByProjectName.Uncertain,
            howLikelyProjectIsLibrary "LibreOfficeProg"
        )

    [<Test>]
    member this.``Likely to be library if contains ends with "lib" (case-insensitive)``() =
        Assert.AreEqual(
            LibraryHeuristicResultByProjectName.Likely,
            howLikelyProjectIsLibrary "FooLib"
        )

    [<Test>]
    member this.``Unlikely to be library if contains "CLI" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByProjectName.Unlikely,
            howLikelyProjectIsLibrary "FooCLI"
        )

    [<Test>]
    member this.``Uncertain to be library if contains "cli" in name not related to CLI``() =
        Assert.AreEqual(
            LibraryHeuristicResultByProjectName.Uncertain,
            howLikelyProjectIsLibrary "InclinedDriver"
        )

    [<Test>]
    member this.``Likely to be library if it starts with "lib", e.g. camelCase``() =
        Assert.AreEqual(
            LibraryHeuristicResultByProjectName.Likely,
            howLikelyProjectIsLibrary "libFoo"
        )

    [<Test>]
    member this.``Unlikely to be library if it contains "console", but segments are separated by dots``() =
        Assert.AreEqual(
            LibraryHeuristicResultByProjectName.Unlikely,
            howLikelyProjectIsLibrary "foo.console.app"
        )
