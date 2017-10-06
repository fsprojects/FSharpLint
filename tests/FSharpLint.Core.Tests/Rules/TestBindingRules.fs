module TestBindingRules

open NUnit.Framework
open FSharpLint.Rules.Binding
open FSharpLint.Framework.Configuration

let config = 
    let ruleEnabled = { Rule.Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }

    Map.ofList 
        [ (AnalyserName, 
            { Rules = Map.ofList 
                [ ("FavourIgnoreOverLetWild", ruleEnabled) 
                  ("UselessBinding", ruleEnabled) 
                  ("WildcardNamedWithAsPattern", ruleEnabled) 
                  ("TupleOfWildcards", ruleEnabled) ]
              Settings = Map.ofList [ ("Enabled", Enabled(true)) ] }) ]
              
[<TestFixture>]
type TestBindingRules() =
    inherit TestRuleBase.TestRuleBase(analyser, config)

    [<Test>]
    member this.LetWildcardUnitValue() = 
        this.Parse """
module Program

let _ = ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.LetWildcardUnitValueSuppressed() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("Binding", "FavourIgnoreOverLetWild")>]
let _ = ()"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.LetWildcardMultilaneStatementOfUnit() = 
        this.Parse """
module Program

let (_) = 
  let x = 4 + 4
  ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.LetWildCardInParanUnitValue() = 
        this.Parse """
module Program

let ((((_)))) = List.iter (fun x -> ()) []"""

        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.LetNonWildcardUnitValue() = 
        this.Parse """
module Program

let a = List.iter (fun x -> ()) []"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test; Category("NetstandardKnownFailure")>]
    member this.UselessBinding() = 
        this.Parse("""
module Program

let a = 10
let a = a""", checkInput = true)

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.NotUselessBindingAsShadowingMutableWithImmutable() = 
        this.Parse """
module Program

let mutable a = 10
let a = a"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.NotUselessBindingAsShadowingImmutableWithMutable() = 
        this.Parse """
module Program

let a = 10
let mutable a = a"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.UselessBindingSuppressed() = 
        this.Parse """
module Program

let a = 10
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Binding", "UselessBinding")>]
let a = a"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test; Category("NetstandardKnownFailure")>]
    member this.UselessBindingWithParens() = 
        this.Parse("""
module Program

let a = 10
let ((a)) = ((a))""", checkInput = true)

        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    /// Regression test for https://github.com/fsprojects/FSharpLint/issues/101
    /// (a use binding will dispose the value so is not useless)
    [<Test>]
    member this.UseBindingWithSameNameDoesNotCauseUselessBindingError() = 
        this.Parse("""
module Program

type Cat() =
    static member CreateList(reader:TextReader) = 
        use reader = reader
        reader.ReadToEnd()""", checkInput = true)
        
        Assert.IsFalse(this.ErrorsExist)
        
    [<Test>]
    member this.WildcardNamedWithAsPattern() = 
        this.Parse """
module Program

match [] with
    | _ as x -> ()"""

        Assert.IsTrue(this.ErrorExistsAt(5, 6))
        
    [<Test>]
    member this.WildcardNamedWithAsPatternSuppressed() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("Binding", "WildcardNamedWithAsPattern")>]
let f =
    match [] with
        | _ as x -> ()"""
        
        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.NamedPattern() = 
        this.Parse """
module Program

match [] with
    | x -> ()"""
    
        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.TupleOfWildcards() = 
        this.Parse """
module Program

type Cat = | Persian of int * int

match Persian(1, 3) with
    | Persian(_, _) -> ()"""

        Assert.IsTrue(this.ErrorExistsAt(7, 14))

    [<Test>]
    member this.``Method's parameter list of wildcards should not be treated as tuple of wildcards.``() = 
        this.Parse """
module Program

type Cat() = 
    member __.Persian(_, _) = ()"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Constructor's parameter list of wildcards should not be treated as tuple of wildcards.``() = 
        this.Parse """
module Program

type Cat() = 
    new(_, _) = Cat()

    member __.Persian(_) = ()"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Method with type argument's parameter list of wildcards should not be treated as tuple of wildcards.``() = 
        this.Parse """
module Program

type Cat() = 
    member __.Persian<'t>(_, _) = ()"""

        Assert.IsFalse(this.ErrorsExist)

    [<Test>]
    member this.``Method's parameter list of wildcards in object expressions should not be treated as tuple of wildcards.``() =
        this.Parse """
module Program

type I =
    abstract member Two : bool * bool -> bool

let x =
    { new I with
        member __.Two(_, _) = false }"""

        Assert.IsFalse(this.ErrorsExist)