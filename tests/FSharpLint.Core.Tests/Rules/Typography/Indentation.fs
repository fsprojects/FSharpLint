module FSharpLint.Core.Tests.Rules.Typography.Indentation

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestTypographyIndentation() =
    inherit TestIndentationRuleBase.TestIndentationRuleBase(Indentation.rule)

    [<Test>]
    member this.``Violation for incorrect indentation``() =
        this.Parse """
module P

let x =
  x
"""

        Assert.IsTrue(this.ViolationExistsAt(5, 0))

    [<Test>]
    member this.``No violation for correct indentation``() =
        this.Parse """
module P

let x =
    x
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``Violation for incorrect record field indentation``() =
        this.Parse """
module P

let rainbow =
    { X = "X"
          Y = "Y"}
"""

        Assert.IsTrue(this.ViolationExistsAt(6, 0))

    [<Test>]
    member this.``No violation for correct record field indentation``() =
        this.Parse """
module P

let rainbow =
    { X = "X"
      Y = "Y"}
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for correct record field indentation in type definition``() =
        this.Parse """
type T =
    { X : int
      Y : string }
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for correct record field indentation with multiple fields per line``() =
        this.Parse """
module P

let rainbow =
    { X = "X"; Z = "Z"
      Y = "Y"; W = "W"}
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for correct multiline record field indentation``() =
        this.Parse """
module P

let rainbow =
    { X =
        (fun x ->
            x + 1)
      Y = "Y"}
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for correct array member indentation``() =
        this.Parse """
module P

let pascalsTriangle =
    [| 1
       2
       3
    |]
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for multi-line tuple``() =
        this.Parse """
module P

let pascalsTriangle =
    (1,
     2,
     3)
"""

        Assert.IsTrue(this.NoViolationsExist)


    [<Test>]
    member this.``No violation for correct list member indentation``() =
        this.Parse """
module P

let pascalsTriangle =
    [ 1
      2
      3
    ]
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for exceptional pipeline indentation``() =
        this.Parse """
module P

let res = 1
          |> add 2
          |> sub 3
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for exceptional composition indentation``() =
        this.Parse """
module P

let res = add 2
          >> sub 3
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for exceptional infix indentation``() =
        this.Parse """
module P

let res = 1
          + 2
          + 3
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for pipeline on same line``() =
        this.Parse """
module P

let res = 1 |> add 2
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for standard pipeline indentation``() =
        this.Parse """
module P

let res =
    1
    |> add 2
    |> sub 3
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for exceptional multiline pipeline indentation``() =
        this.Parse """
module P

let res = 1
          |> (fun x ->
              x + 1)
          |> (fun x ->
              x - 3)
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for exceptional object expression indentation``() =
        this.Parse """
module P

let comparer =
    { new IComparer<string> with
          member x.Compare(s1, s2) =
              let rev (s : String) =
                  new String (Array.rev (s.ToCharArray()))
              let reversed = rev s1 i
              reversed.CompareTo (rev s2) }
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for record with comment``() =
        this.Parse """
    type CurrentNode =
        { Node: AstNode
          ChildNodes: AstNode list

          /// A list of parent nodes e.g. parent, grand parent, grand grand parent.
          Breadcrumbs: AstNode list }
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for exceptional nested records indentation``() =
        this.Parse """
    { LoadedRules.GlobalConfig = getGlobalConfig config.Global
      DeprecatedRules = deprecatedAllRules
      AstNodeRules = astNodeRules.ToArray()
      LineRules =
        { GenericLineRules = lineRules.ToArray()
          IndentationRule = indentationRule
          NoTabCharactersRule = noTabCharactersRule } }
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for exceptional list indentation with multiple items per line``() =
        this.Parse """
let opchars =
    [ '>';'<';'+';'-';'*';'=';'~';'%';'&';'|';'@'
      '#';'^';'!';'?';'/';'.';':';',' ]
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for exceptional match guard indentation``() =
        this.Parse """
match args.AstNode with
| AstNode.Binding(SynBinding(_, _, _, isMutable, _, _, _, pattern, _, expr, range, _))
        when Helper.Binding.isLetBinding args.NodeIndex args.SyntaxArray args.SkipArray
             && not isMutable ->
    checkForUselessBinding args.CheckInfo pattern expr range
| _ ->
    Array.empty
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for exceptional tuple pattern indentation``() =
        this.Parse """
    let (|Cons|_|) pattern =
        match pattern with
        | SynPat.LongIdent(LongIdentWithDots([identifier], _),
                           _, _,
                           Pats([SynPat.Tuple(_, [lhs; rhs], _)]), _, _)
                when identifier.idText = "op_ColonColon" ->
            Some(lhs, rhs)
        | _ -> None
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for exceptional record pattern indentation``() =
        this.Parse """
        match x with
        | X { y = y
              z = z } -> ()
"""

        Assert.IsTrue(this.NoViolationsExist)

    [<Test>]
    member this.``No violation for multi-line reference cell assignment``() =
        this.Parse """
        do pstringelemImpl :=
            choice
                [ attempt pstringchar
                  skipChar '\\' >>. pnewline >>. many spaces >>. pstringelem ]
"""

        Assert.IsTrue(this.NoViolationsExist)
