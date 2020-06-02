module TestSuppression

open NUnit.Framework
open FSharpLint.Framework

let rules = Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"]

let lines text =
    FSharpLint.Framework.String.toLines text |> Array.map (fun (line, _, _) -> line)

[<TestFixture>]
type internal TestSuppression() =
    [<Test>]
    member __.``Disable next line with rules specified suppresses as expected``() =
        let text = lines """
// fsharplint:disable-next-line TypePrefixing TypedItemSpacing


"""

        let suppressionInfo = Suppression.parseSuppressionInfo rules text

        Assert.IsTrue(Suppression.isSuppressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 3 suppressionInfo)

        Assert.IsFalse(Suppression.isSuppressed "TypePrefixing" 4 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TypedItemSpacing" 4 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 4 suppressionInfo)

    [<Test>]
    member __.``Disable next line with no rules specified suppresses as expected``() =
        let text = lines """
// fsharplint:disable-next-line


"""

        let suppressionInfo = Suppression.parseSuppressionInfo rules text

        Assert.IsTrue(Suppression.isSuppressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TupleCommaSpacing" 3 suppressionInfo)

        Assert.IsFalse(Suppression.isSuppressed "TypePrefixing" 4 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TypedItemSpacing" 4 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 4 suppressionInfo)

    [<Test>]
    member __.``Disable current line with rules specified suppresses as expected``() =
        let text = lines """
// fsharplint:disable-line TypePrefixing TypedItemSpacing

"""

        let suppressionInfo = Suppression.parseSuppressionInfo rules text

        Assert.IsTrue(Suppression.isSuppressed "TypePrefixing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TypedItemSpacing" 2 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 2 suppressionInfo)

        Assert.IsFalse(Suppression.isSuppressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 3 suppressionInfo)

    [<Test>]
    member __.``Disable current line with no rules specified suppresses as expected``() =
        let text = lines """
// fsharplint:disable-line

"""

        let suppressionInfo = Suppression.parseSuppressionInfo rules text

        Assert.IsTrue(Suppression.isSuppressed "TypePrefixing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TypedItemSpacing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TupleCommaSpacing" 2 suppressionInfo)

        Assert.IsFalse(Suppression.isSuppressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 3 suppressionInfo)

    [<Test>]
    member __.``Disable and re-renable with no rules specified suppresses as expected``() =
        let text = lines """
// fsharplint:disable


// fsharplint:enable

"""

        let suppressionInfo = Suppression.parseSuppressionInfo rules text

        Assert.IsTrue(Suppression.isSuppressed "TypePrefixing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TypedItemSpacing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TupleCommaSpacing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TupleCommaSpacing" 3 suppressionInfo)

        Assert.IsFalse(Suppression.isSuppressed "TypePrefixing" 5 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TypedItemSpacing" 5 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 5 suppressionInfo)

    [<Test>]
    member __.``Disable and re-renable with rules specified suppresses as expected``() =
        let text = lines """
// fsharplint:disable TypePrefixing  TypedItemSpacing


// fsharplint:enable TypePrefixing

"""

        let suppressionInfo = Suppression.parseSuppressionInfo rules text

        Assert.IsTrue(Suppression.isSuppressed "TypePrefixing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TypedItemSpacing" 2 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 3 suppressionInfo)

        Assert.IsFalse(Suppression.isSuppressed "TypePrefixing" 5 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TypedItemSpacing" 5 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 5 suppressionInfo)

    [<Test>]
    member __.``Disable line within disable section suppresses as expected``() =
        let text = lines """
// fsharplint:disable TypePrefixing

// fsharplint:disable-next-line TypedItemSpacing


"""

        let suppressionInfo = Suppression.parseSuppressionInfo rules text

        Assert.IsTrue(Suppression.isSuppressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 3 suppressionInfo)

        Assert.IsTrue(Suppression.isSuppressed "TypePrefixing" 5 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TypedItemSpacing" 5 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 5 suppressionInfo)

        Assert.IsTrue(Suppression.isSuppressed "TypePrefixing" 6 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TypedItemSpacing" 6 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 6 suppressionInfo)

    [<Test>]
    member __.``Disable within disable section suppresses as expected``() =
        let text = lines """
// fsharplint:disable TypePrefixing

// fsharplint:disable TypedItemSpacing

// fsharplint:enable

"""

        let suppressionInfo = Suppression.parseSuppressionInfo rules text

        Assert.IsTrue(Suppression.isSuppressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 3 suppressionInfo)

        Assert.IsTrue(Suppression.isSuppressed "TypePrefixing" 5 suppressionInfo)
        Assert.IsTrue(Suppression.isSuppressed "TypedItemSpacing" 5 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 5 suppressionInfo)

        Assert.IsFalse(Suppression.isSuppressed "TypePrefixing" 7 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TypedItemSpacing" 7 suppressionInfo)
        Assert.IsFalse(Suppression.isSuppressed "TupleCommaSpacing" 7 suppressionInfo)
