module TestSuppression

open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open NUnit.Framework
open FSharpLint.Framework
open FSharpLint.Framework.Suppression

[<TestFixture>]
type TestSuppression() =

    [<Test>]
    member __.``suppression comments are correctly parsed``() =
        let text = """
// fsharplint:disable

(* fsharplint:enable *)

// fsharplint:disable TypePrefixing TypedItemSpacing

// fsharplint:enable TypePrefixing TypedItemSpacing

// fsharplint:disable-next-line

// fsharplint:disable-next-line TypePrefixing TypedItemSpacing

// fsharplint:enable-next-line

// fsharplint:enable-next-line TypePrefixing TypedItemSpacing

// fsharplint:enable-line

// fsharplint:disable-line

// fsharplint:enable-line TypePrefixing TypedItemSpacing

// fsharplint:disable-line TypePrefixing TypedItemSpacing"""

        let parseResult = Suppression.parseSuppressionInfo (FSharpLint.Framework.String.toLines text |> Array.map (fun (line, _, _) -> line) |> Array.toList)
        Assert.AreEqual(
            [|
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
                (16, None)
                (17, Some (SuppressionInfo.EnableLine All))
                (18, None)
                (19, Some (SuppressionInfo.DisableLine All))
                (20, None)
                (21, Some (SuppressionInfo.EnableLine (Rules (Set.ofList ["TypePrefixing"; "TypedItemSpacing"]))))
                (22, None)
                (23, Some (SuppressionInfo.DisableLine (Rules (Set.ofList ["TypePrefixing"; "TypedItemSpacing"]))))
            |],
            parseResult)

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

// fsharplint:enable-line TypePrefixing TypedItemSpacing

// fsharplint:enable-line

// fsharplint:enable

// fsharplint:disable-next-line

// fsharplint:disable-next-line TypePrefixing TypedItemSpacing

// fsharplint:disable-line TypePrefixing TypedItemSpacing

// fsharplint:disable-line"""

        let allRules = Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"]
        let lines = FSharpLint.Framework.String.toLines text |> Array.map (fun (line, _, _) -> line) |> Array.toList
        let result = Suppression.getSuppressedRulesPerLine allRules lines
        let expected = dict [|
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
            (16, Set.ofList ["TupleCommaSpacing"])
            (17, Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"])
            (18, Set.empty)
            (19, Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"])
            (20, Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"])
            (21, Set.empty)
            (22, Set.empty)
            (23, Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"])
            (24, Set.empty)
            (25, Set.ofList ["TypePrefixing"; "TypedItemSpacing"])
            (26, Set.ofList ["TypePrefixing"; "TypedItemSpacing"])
            (27, Set.empty)
            (28, Set.ofList ["TypePrefixing"; "TypedItemSpacing"; "TupleCommaSpacing"])
        |]

        CollectionAssert.AreEquivalent(expected, result)


    [<Test>]
    member __.``suppression comments work to suppress warnings``() =
        let text = """
// fsharplint:disable-next-line
let x = not true

// fsharplint:disable-next-line Hints
let y = not true

let a = not true // fsharplint:disable-line

// fsharplint:disable

let z = not true

let a = not true // fsharplint:enable-line

// fsharplint:enable

let a = not true"""

        let parsedFileInfo =
            match ParseFile.parseSource text (FSharpChecker.Create()) with
            | ParseFile.Success(parseFileInformation) ->
                { Source = parseFileInformation.Text
                  Ast = parseFileInformation.Ast
                  TypeCheckResults = parseFileInformation.TypeCheckResults }
            | ParseFile.Failed _ -> failwith "Failed to parse"

        let warnings =
            match Lint.lintParsedFile OptionalLintParameters.Default parsedFileInfo "" with
            | LintResult.Success warnings -> warnings
            | LintResult.Failure _ -> failwith "Failed to lint"
        Assert.AreEqual(2, warnings.Length)
        Assert.AreEqual(14, warnings.[0].Details.Range.StartLine)
        Assert.AreEqual(18, warnings.[1].Details.Range.StartLine)

