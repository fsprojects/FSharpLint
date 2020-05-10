module TestSuppression

open NUnit.Framework
open FSharpLint.Framework

let rules = Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"]

let lines text = 
    FSharpLint.Framework.String.toLines text |> Array.map (fun (line, _, _) -> line) |> Array.toList

[<TestFixture>]
type TestSuppression() =
    [<Test>]
    member __.``Disable next line with rules specified supresses as expected``() =
        let text = lines """
// fsharplint:disable-next-line TypePrefixing TypedItemSpacing


"""

        let suppressionInfo = Suppression.parseSuppressionInfo rules text
        
        Assert.IsTrue(Suppression.isSupressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 3 suppressionInfo)

        Assert.IsFalse(Suppression.isSupressed "TypePrefixing" 4 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TypedItemSpacing" 4 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 4 suppressionInfo)

    [<Test>]
    member __.``Disable next line with no rules specified supresses as expected``() =
        let text = lines """
// fsharplint:disable-next-line


"""

        let suppressionInfo = Suppression.parseSuppressionInfo rules text
            
        Assert.IsTrue(Suppression.isSupressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TupleCommaSpacing" 3 suppressionInfo)

        Assert.IsFalse(Suppression.isSupressed "TypePrefixing" 4 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TypedItemSpacing" 4 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 4 suppressionInfo)

    [<Test>]
    member __.``Disable current line with rules specified supresses as expected``() =
        let text = lines """
// fsharplint:disable-line TypePrefixing TypedItemSpacing

"""

        let suppressionInfo = Suppression.parseSuppressionInfo rules text
        
        Assert.IsTrue(Suppression.isSupressed "TypePrefixing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TypedItemSpacing" 2 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 2 suppressionInfo)

        Assert.IsFalse(Suppression.isSupressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 3 suppressionInfo)
        
    [<Test>]
    member __.``Disable current line with no rules specified supresses as expected``() =
        let text = lines """
// fsharplint:disable-line
        
"""
        
        let suppressionInfo = Suppression.parseSuppressionInfo rules text
                
        Assert.IsTrue(Suppression.isSupressed "TypePrefixing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TypedItemSpacing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TupleCommaSpacing" 2 suppressionInfo)
        
        Assert.IsFalse(Suppression.isSupressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 3 suppressionInfo)
        
    [<Test>]
    member __.``Disable and re-renable with no rules specified supresses as expected``() =
        let text = lines """
// fsharplint:disable


// fsharplint:enable
        
"""
        
        let suppressionInfo = Suppression.parseSuppressionInfo rules text
                
        Assert.IsTrue(Suppression.isSupressed "TypePrefixing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TypedItemSpacing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TupleCommaSpacing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TupleCommaSpacing" 3 suppressionInfo)
        
        Assert.IsFalse(Suppression.isSupressed "TypePrefixing" 5 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TypedItemSpacing" 5 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 5 suppressionInfo)
        
    [<Test>]
    member __.``Disable and re-renable with rules specified supresses as expected``() =
        let text = lines """
// fsharplint:disable TypePrefixing  TypedItemSpacing


// fsharplint:enable TypePrefixing
        
"""
        
        let suppressionInfo = Suppression.parseSuppressionInfo rules text
                
        Assert.IsTrue(Suppression.isSupressed "TypePrefixing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TypedItemSpacing" 2 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 2 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 3 suppressionInfo)
        
        Assert.IsFalse(Suppression.isSupressed "TypePrefixing" 5 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TypedItemSpacing" 5 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 5 suppressionInfo)
        
    [<Test>]
    member __.``Disable line within disable section supresses as expected``() =
        let text = lines """
// fsharplint:disable TypePrefixing  

// fsharplint:disable-next-line TypedItemSpacing

        
"""
        
        let suppressionInfo = Suppression.parseSuppressionInfo rules text
                
        Assert.IsTrue(Suppression.isSupressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 3 suppressionInfo)

        Assert.IsTrue(Suppression.isSupressed "TypePrefixing" 5 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TypedItemSpacing" 5 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 5 suppressionInfo)
        
        Assert.IsTrue(Suppression.isSupressed "TypePrefixing" 6 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TypedItemSpacing" 6 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 6 suppressionInfo)
        
    [<Test>]
    member __.``Disable within disable section supresses as expected``() =
        let text = lines """
// fsharplint:disable TypePrefixing  

// fsharplint:disable TypedItemSpacing

// fsharplint:enable
        
"""
        
        let suppressionInfo = Suppression.parseSuppressionInfo rules text
                
        Assert.IsTrue(Suppression.isSupressed "TypePrefixing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TypedItemSpacing" 3 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 3 suppressionInfo)
        
        Assert.IsTrue(Suppression.isSupressed "TypePrefixing" 5 suppressionInfo)
        Assert.IsTrue(Suppression.isSupressed "TypedItemSpacing" 5 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 5 suppressionInfo)
                
        Assert.IsFalse(Suppression.isSupressed "TypePrefixing" 7 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TypedItemSpacing" 7 suppressionInfo)
        Assert.IsFalse(Suppression.isSupressed "TupleCommaSpacing" 7 suppressionInfo)
