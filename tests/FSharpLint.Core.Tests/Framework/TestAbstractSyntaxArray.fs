module TestAbstractSyntaxArray

open System.Diagnostics
open FSharpLint.Framework
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AbstractSyntaxArray
open FSharp.Compiler.Syntax
open Microsoft.FSharp.Reflection
open NUnit.Framework
open TestUtils
open FSharpLint.Framework.ExpressionUtilities

[<TestFixture>]
type TestAst() =
    let unionCaseName (x:'T) =
        match FSharpValue.GetUnionFields(x, typeof<'T>) with
        | case, _ -> case.Name

    let astToExpr ast =
        let (|Module|_|) x =
            match x with
            | SynModuleOrNamespace(_, _, _, SynModuleDecl.Expr(app, _)::_, _, _, _, _, _) ->
                Some(app)
            | _ -> None

        match ast with
        | ParsedInput.ImplFile(x) ->
            match x with
            | ParsedImplFileInput(_, _, _, _, _, Module(app)::_, _, _) ->
                app
            | _ -> failwith "Expected at least one module or namespace."
        | _ -> failwith "Expected an implementation file."

    let astNodeName = removeParens >> unionCaseName

    [<Test>]
    member __.``Flatten with right pipe adds lhs to end of function application.``() =
        match generateAst "x |> List.map (fun x -> x)" |> astToExpr |> Expression with
        | FuncApp(expressions, _) ->
            Assert.AreEqual(["LongIdent"; "Lambda"; "Ident"], expressions |> List.map astNodeName)
        | _ -> Assert.Fail()

    [<Test>]
    member __.``Flatten with left pipe adds rhs to end of function application.``() =
        match generateAst "List.map (fun x -> x) <| x" |> astToExpr |> Expression with
        | FuncApp(expressions, _) ->
            Assert.AreEqual(["LongIdent"; "Lambda"; "Ident"], expressions |> List.map astNodeName)
        | _ -> Assert.Fail()

    [<Test>]
    member __.``Flatten with right pipe adds lhs to end of function application no matter the number of arguments on rhs.``() =
        match generateAst "x |> List.map (fun x -> x) 1" |> astToExpr |> Expression with
        | FuncApp(expressions, _) ->
            Assert.AreEqual(["LongIdent"; "Lambda"; "Const"; "Ident"], expressions |> List.map astNodeName)
        | _ -> Assert.Fail()

    [<Test>]
    member __.``Flatten with binary operator on lhs of right pipe.``() =
        match generateAst "x::[] |> List.map (fun x -> x)" |> astToExpr |> Expression with
        | FuncApp(expressions, _) ->
            Assert.AreEqual(["LongIdent"; "Lambda"; "App"], expressions |> List.map astNodeName)
        | _ -> Assert.Fail()

    [<Test>]
    member __.``Flatten with function application on lhs of right pipe.``() =
        match generateAst "foo x |> List.map (fun x -> x)" |> astToExpr |> Expression with
        | FuncApp(expressions, _) ->
            Assert.AreEqual(["LongIdent"; "Lambda"; "App"], expressions |> List.map astNodeName)
        | _ -> Assert.Fail()

    [<Test>]
    member __.``Flatten with multiple right pipes.``() =
        match generateAst "x |> foo |> List.map (fun x -> x)" |> astToExpr |> Expression with
        | FuncApp(expressions, _) ->
            Assert.AreEqual(["LongIdent"; "Lambda"; "App"], expressions |> List.map astNodeName)
        | _ -> Assert.Fail()

    [<Test>]
    member __.``Flatten with multiple left pipes.``() =
        match generateAst "List.map (fun x -> x) <| 1 <| x" |> astToExpr |> Expression with
        | FuncApp(expressions, _) ->
            Assert.AreEqual(["LongIdent"; "Lambda"; "Const"; "Ident"], expressions |> List.map astNodeName)
        | _ -> Assert.Fail()

    [<Category("Performance")>]
    [<Test>]
    member __.``Performance of building syntax array``() =
        let (tree, _) = getPerformanceTestInput ()

        let iterations = 100

        let stopwatch = Stopwatch.StartNew()
        let times = ResizeArray()

        for _ in 0..iterations do
            stopwatch.Restart()

            astToArray tree |> ignore

            stopwatch.Stop()

            times.Add stopwatch.ElapsedMilliseconds

        let result = times |> Seq.sum |> (fun totalMilliseconds -> totalMilliseconds / int64 iterations)

        Assert.Less(result, 200)
        System.Console.WriteLine(sprintf "Built array in an average of %d milliseconds." result)

    [<Test>]
    member __.``Syntax array constructed from AST in valid order.``() =
        let tree = generateAst "List.map (fun x y -> id x) woofs"

        let array = astToArray tree

        let actual = array |> Array.map (fun x -> x.Hashcode) |> Array.toList

        let expected =
            [ Utilities.hash2 SyntaxNode.ModuleOrNamespace 0
              Utilities.hash2 SyntaxNode.ModuleDeclaration 0
              Utilities.hash2 SyntaxNode.FuncApp 0
              Utilities.hash2 SyntaxNode.Identifier "map"
              Utilities.hash2 SyntaxNode.Paren 0
              Utilities.hash2 SyntaxNode.Lambda 0
              Utilities.hash2 SyntaxNode.LambdaArg 0
              Utilities.hash2 SyntaxNode.Identifier "x"
              Utilities.hash2 SyntaxNode.LambdaArg 0
              Utilities.hash2 SyntaxNode.Identifier "y"
              Utilities.hash2 SyntaxNode.LambdaBody 0
              Utilities.hash2 SyntaxNode.FuncApp 0
              Utilities.hash2 SyntaxNode.Identifier "id"
              Utilities.hash2 SyntaxNode.Identifier "x"
              Utilities.hash2 SyntaxNode.Identifier "woofs" ]

        Assert.AreEqual(expected, actual)
        
        let expected = array |> Array.map (fun x -> (x.NumberOfChildren, x.ParentIndex)) |> Array.toList
        Assert.AreEqual([ (14, 0)
                          (13, 0)
                          (12, 1)
                          (0, 2)
                          (9, 2)
                          (8, 4)
                          (1, 5)
                          (0, 6)
                          (1, 5)
                          (0, 8)
                          (3, 5)
                          (2, 10)
                          (0, 11)
                          (0, 11)
                          (0, 2) ],
                        expected)

    /// e.g. a lambda arg shouldn't have the body of the lambda in its child nodes (that should be a sibling).
    [<Test>]
    member __.``Syntax array's extra info nodes do not contain children of node they're generated from.``() =
        let tree = generateAst "fun x -> x"

        let array = astToArray tree

        let actual = array |> Array.map (fun x -> x.Hashcode) |> List.ofArray

        let expected =
            [ Utilities.hash2 SyntaxNode.ModuleOrNamespace 0
              Utilities.hash2 SyntaxNode.ModuleDeclaration 0
              Utilities.hash2 SyntaxNode.Lambda 0
              Utilities.hash2 SyntaxNode.LambdaArg 0
              Utilities.hash2 SyntaxNode.Identifier "x"
              Utilities.hash2 SyntaxNode.LambdaBody 0
              Utilities.hash2 SyntaxNode.Identifier "x" ]

        Assert.AreEqual(expected, actual)

        let expected = array |> Array.map (fun x -> (x.NumberOfChildren, x.ParentIndex)) |> List.ofArray
        Assert.AreEqual([ (6, 0)
                          (5, 0)
                          (4, 1)
                          (1, 2)
                          (0, 3)
                          (1, 2)
                          (0, 5) ],
                        expected)