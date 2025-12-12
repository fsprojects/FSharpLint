module FSharpLint.Core.Tests.Rules.Conventions.PublicValuesNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules
open FSharpLint.Core.Tests

let config =
    { NamingConfig.Naming = None
      Underscores = Some NamingUnderscores.AllowPrefix
      Prefix = None
      Suffix = None }
[<TestFixture>]
type TestConventionsPublicValuesNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(PublicValuesNames.rule config)

    /// A public binding let binding identifier may be pascal case or upper case.
    [<Test>]
    member this.PublicTupleIsPascalCase() =
        this.Parse """
module Program

let (Cat, _) = 1, 0
"""

        this.AssertNoViolations()

    /// Extern functions typically match the counterpart from the external library
    /// and as such often won't follow F#'s conventions.
    [<Test>]
    member this.``extern function definition should not trigger naming violations``() =
        this.Parse """
module Program

open System
open System.Runtime.InteropServices

[<DllImport(@"python37", EntryPoint = "PyThreadState_SetAsyncExc", CallingConvention = CallingConvention.Cdecl)>]
extern int PyThreadState_SetAsyncExcLLP64(uint id, IntPtr exc)
"""

        this.AssertNoViolations()

    [<Test>]
    member this.PublicTupleIsCamelCase() =
        this.Parse """
module Program

let (cat, _) = 1, 0
"""

        this.AssertNoViolations()

    /// A public binding let binding identifier may be pascal case or upper case.
    [<Test>]
    member this.PublicFunctionNameIsPascalCase() =
        this.Parse """
module Program

let Main () = ()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.PublicFunctionNameIsCamelCase() =
        this.Parse """
module Program

let main () = ()
"""

        this.AssertNoViolations()

    [<Test>]
    member this.UnionCaseInBindingContainingValueDoesNotGenerateViolation() =
        this.Parse """
module Program

type SingleCaseDU = SingleCaseDU of int

let (SingleCaseDU myInt) = (SingleCaseDU 5)
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``ActivePatternDoesNotGenerateViolations`` () =
         this.Parse """
let (|Empty|_|) str =
    match str with
    | "" -> Some Empty
    | _ -> None
"""

         this.AssertNoViolations()
