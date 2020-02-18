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
        // TODO: text
        let text = """
"""

        let allRules = Set.empty // TODO: get reference to all rule identifiers
        let lines = String.getLines text
        Suppression.getSuppressedRulesPerLine allRules lines
