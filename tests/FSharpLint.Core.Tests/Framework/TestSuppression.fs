module TestSuppression

open FSharp.Compiler.AbstractIL.Internal.Library
open NUnit.Framework
open FSharpLint.Framework
open FSharpLint.Framework.Suppression

[<TestFixture>]
type TestSuppression() =

    [<Test>]
    member __.``suppression comments are correctly parsed``() =
        let text = """
// fsharplint:disable

// fsharplint:enable

// fsharplint:disable TypePrefixing TypedItemSpacing

// fsharplint:enable TypePrefixing TypedItemSpacing

// fsharplint:disable-next-line

// fsharplint:disable-next-line TypePrefixing TypedItemSpacing

// fsharplint:enable-next-line

// fsharplint:enable-next-line TypePrefixing TypedItemSpacing"""

        let parseResult = Suppression.parseSuppressionInfo (String.getLines text)
        Assert.AreEqual([|
            (0, None)
            (1, Some (SuppressionInfo.Disable All))
            (2, None)
            (3, Some (SuppressionInfo.Enable All))
            (4, None)
            (5, Some (SuppressionInfo.Disable (Rules (Set.ofList ["TypePrefixing"; "TypedItemSpacing"]))))
            (6, None)
            (7, Some (SuppressionInfo.Enable (Rules (Set.ofList ["TypePrefixing"; "TypedItemSpacing"]))))
            (8, None)
            (9, Some (SuppressionInfo.DisableNextLine All))
            (10, None)
            (11, Some (SuppressionInfo.DisableNextLine (Rules (Set.ofList ["TypePrefixing"; "TypedItemSpacing"]))))
            (12, None)
            (13, Some (SuppressionInfo.EnableNextLine All))
            (14, None)
            (15, Some (SuppressionInfo.EnableNextLine (Rules (Set.ofList ["TypePrefixing"; "TypedItemSpacing"]))))
        |], parseResult)

    [<Test>]
    member __.``suppression info is correctly converted to line suppression``() =
        let text = """

// fsharplint:disable

// fsharplint:enable TypePrefixing TypedItemSpacing

// fsharplint:enable

// fsharplint:disable TypePrefixing TypedItemSpacing

// fsharplint:disable

// fsharplint:enable-next-line TypePrefixing TypedItemSpacing

// fsharplint:enable-next-line

// fsharplint:enable

// fsharplint:disable-next-line

// fsharplint:disable-next-line TypePrefixing TypedItemSpacing

"""

        let allRules = Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"]
        let lines = String.getLines text
        let result = Suppression.getSuppressedRulesPerLine allRules lines
        CollectionAssert.AreEquivalent(dict [|
            (0, Set.empty)
            (1, Set.empty)
            (2, Set.empty)
            (3, Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"])
            (4, Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"])
            (5, Set.ofList ["TupleCommaSpacing"])
            (6, Set.ofList ["TupleCommaSpacing"])
            (7, Set.empty)
            (8, Set.empty)
            (9, Set.ofList ["TypePrefixing"; "TypedItemSpacing"])
            (10, Set.ofList ["TypePrefixing"; "TypedItemSpacing"])
            (11, Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"])
            (12, Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"])
            (13, Set.ofList ["TupleCommaSpacing"])
            (14, Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"])
            (15, Set.empty)
            (16, Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"])
            (17, Set.empty)
            (18, Set.empty)
            (19, Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"])
            (20, Set.empty)
            (21, Set.ofList ["TypePrefixing"; "TypedItemSpacing"])
            (22, Set.empty)
        |], result)

