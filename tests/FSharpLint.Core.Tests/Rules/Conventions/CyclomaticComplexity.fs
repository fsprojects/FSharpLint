module FSharpLint.Core.Tests.Rules.Conventions.CyclomaticComplexity

open NUnit.Framework
open FSharpLint.Rules.CyclomaticComplexity

/// The max cyclomatic complexity as configured in these tests.
[<Literal>]
let private MaxComplexity = 5

/// '\n' character as a string. Used instead of Environment.NewLine because line breaks in triple-comment strings are not equivalent to Environment.NewLine on Windows.
[<Literal>]
let private NewLine = "\n"

/// Indent all lines of a string equally by the given number of spaces.
let private indent numSpaces (s: string) =
    let indentStr = String.replicate numSpaces " "
    let result = indentStr + s.Replace(NewLine, $"{NewLine}{indentStr}")
    result
    
/// Generates a body of code containing a match expression.
let private makeMatchSnippet len =
    $"""match "dummyString" with
{Seq.map (fun i -> (sprintf "| \"%d\" -> ()" i)) [| 1..len-1 |] |> String.concat NewLine}
| _ -> ()"""

/// Generates a body of code containing a match expression with a when clause containing a logical operator in each pattern.
let private makeMatchSnippetWithLogicalOperatorsInWhenClause len =
    $"""match "dummyString" with
{Seq.map (fun i -> (sprintf "| x when x = \"%d\" || x = \"%d\" -> ()" (i*len) (i*len+1))) [| 1..len-1 |] |> String.concat NewLine}
| _ -> ()"""
    
/// module declaration and let binding declaration for a body of code
let private makeProgram funcString body =
    $"""Module Program
let {funcString} =
{indent 4 body}"""

/// Generates a body of code consisting of if-else expressions.
let private ifElseExpressions len =
    $"""if true then ()
    {String.replicate (len-1) (sprintf "%selif true then ()" NewLine)}"""
    |> makeProgram "f()"

/// Generates a body of code containing for expressions.
let private forExpressions len =
    String.replicate len (sprintf "for i = 0 to 1 do ()%s" NewLine)
    |> makeProgram "f()" 
  
/// Generates a body of code containing foreach expressions.    
let private foreachExpressions len =
    String.replicate len (sprintf "for _ in [] do ()%s" NewLine)
    |> makeProgram "f()"

/// Generates a body of code containing while expressions.   
let private whileExpressions len =
    String.replicate len (sprintf "while false do ()%s" NewLine)
    |> makeProgram "f()"

/// Generates a body of code containing a while expression with multiple logical operators in the condition.
let private whileWithBooleanOperatorsInConditionExpressions len =
    if len < 2 then invalidArg (nameof len) "must be > 2"
    $"""while true && {String.replicate (len-2) (sprintf "%sfalse &&" NewLine)} true do ()"""
    |> makeProgram "f()" 

/// Generates a body of code containing an if statement with multiple && conditional operators 
let private ifThenExpressionWithMultipleAndConditionals len =
    if len < 2 then invalidArg (nameof len) "must be > 2"
    $"""if true && {String.replicate (len-2) (sprintf "%sfalse &&" NewLine)} true then ()"""
    |> makeProgram "f()" 

/// Generates a body of code containing an if statement with multiple || conditional operators    
let private ifThenExpressionWithMultipleOrConditionals len =
    if len < 2 then invalidArg (nameof len) "must be > 2"
    $"""if true || {String.replicate (len-2) (sprintf "%sfalse ||" NewLine)} true then ()"""
    |> makeProgram "f()"
    
/// Generates a body of code containing a match expression with multiple patterns.
let private matchExpression len =
    makeMatchSnippet len
    |> makeProgram "f()"
    
/// Generates a body of code containing a match expression with multiple combined patterns.
let private matchExpressionWithCombinedPatterns len =
    $"""match "dummyString" with
{(Seq.map (fun i -> (sprintf "| \"%d\"" i)) [| 1..len-1 |] |> String.concat NewLine)}
| _ -> ()"""
    |> makeProgram "f()"

/// Generates a body of code containing a match function with multiple patterns.
let private matchFunction len =
    $"""    function 
{(Seq.map (fun i -> (sprintf "    | \"%d\"" i)) [| 1..len-1 |] |> String.concat NewLine)} 
    | _ -> ()
f "dummyString" """
    |> makeProgram "f"
   
/// Generates a computational expression with a match! expression containing multiple patterns.
let private matchBang len =
    $"""async {{
    match! async {{ return "dummyString" }} with
{(Seq.map (fun i -> (sprintf "    | \"%d\"" i)) [| 1..len-1 |] |> String.concat NewLine)}
    | _ -> ()
}}"""
    |> makeProgram "a"
     
/// Tests for the cyclomatic complexity rule.
type TestConventionsCyclomaticComplexity() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(rule { Config.MaxComplexity = MaxComplexity; })

    /// Generator for source code test cases which have cyclomatic complexity equal to maxComplexity.
    /// Yields an enumerable of source code snippets.
    static member private AtMaxComplexityTestCasesSource =
        seq {
            let num = MaxComplexity
            yield ifElseExpressions num
            yield forExpressions num
            yield foreachExpressions num
            yield whileExpressions num
            yield matchExpression num
            yield matchExpressionWithCombinedPatterns num
            yield matchFunction num
            yield matchBang num  
            yield ifThenExpressionWithMultipleAndConditionals num
            yield ifThenExpressionWithMultipleOrConditionals num
            yield whileWithBooleanOperatorsInConditionExpressions num
        } |> Seq.map box |> Seq.map Array.singleton // return an enumerable of obj arrays
    
    /// Generator for source code test cases which have cyclomatic complexity greater than maxComplexity.
    /// Yields an enumerable of (source code snippet, errorLocation) pairs.
    static member private FailureCasesSource =
        seq {
            let num = MaxComplexity + 1
            let errorLocation = 2, 4
            yield ifElseExpressions num, errorLocation
            yield forExpressions num, errorLocation
            yield foreachExpressions num, errorLocation
            yield whileExpressions num, errorLocation
            yield matchExpression num, errorLocation
            yield matchExpressionWithCombinedPatterns num, errorLocation
            yield matchFunction num, errorLocation
            yield matchBang num, errorLocation
            yield ifThenExpressionWithMultipleAndConditionals num, errorLocation
            yield ifThenExpressionWithMultipleOrConditionals num, errorLocation
            yield whileWithBooleanOperatorsInConditionExpressions num, errorLocation    
        } |> Seq.map (fun (x, y) -> [| box x; box y |])
 
    /// Verifies that no cyclomatic complexity over-maximum flags are raised on source that has cyclomatic complexity <= maxComplexity.
    [<TestCaseSource(nameof(TestConventionsCyclomaticComplexity.AtMaxComplexityTestCasesSource))>]
    member this.EnsureNoErrorWhenComplexityBelowThreshold(code) =
        this.Parse code
        Assert.IsTrue(this.NoErrorsExist)
      
    /// Verifies that flags are raised on source code that has cyclomatic complexity > maxComplexity.
    [<TestCaseSource(nameof(TestConventionsCyclomaticComplexity.FailureCasesSource))>]
    member this.EnsureErrorWhenComplexityExceedsThreshold(code, errorLocation) =
        this.Parse code
        Assert.IsTrue(this.ErrorExistsAt(errorLocation))
    
    /// Verifies that an error is raised on a match expression which has multiple boolean operator conditions in multiple when clauses.
    [<Test>]
    member this.TestLogicalOperatorsInMatch() =
        let code = $"""Module Program
let f() = 
{makeMatchSnippetWithLogicalOperatorsInWhenClause (MaxComplexity + 1)}"""
        this.Parse code
        Assert.AreEqual(1, this.ErrorRanges.Length) 
    
    /// Verifies that an error is raised on a single match clause with multiple logical operators.
    [<Test>]
    member this.TestMultipleLogicalOperatorsInSingleMatchClause() =
        let code = $"""Module Program 
let f() =
    match "dummyString" with
    | x when x = "1" {[2..MaxComplexity+1] |> List.map (sprintf "|| x = \"%d\"") |> String.concat " "} -> ()
    | _ -> ()"""
        this.Parse code
        Assert.AreEqual(1, this.ErrorRanges.Length)
      
    /// Verifies that no cyclomatic complexity error is flagged when boolean operators are raised outside of conditional branching contexts (sanity check).
    [<Test>]
    member this.EnsureNoErrorWhenLogicalExpressionOutsideCondition() =
        let code = $"""Module Program
    let f() =
        let t = true {String.replicate (MaxComplexity+1) "|| false "} 
        ()"""    
        this.Parse code
        Assert.IsTrue(this.NoErrorsExist)
    
    /// Verifies that the cyclomatic complexity of functions is independent, by checking that a snippet of code with two functions, each with a complexity lower than the threshold but with a sum complexity greater than the threshold, does not flag an error. 
    [<Test>]
    member this.EnsureComplexityOfFunctionsWithLowComplexityAreIndependent() =
        let code = $"""Module Program
    let f() =
{makeMatchSnippetWithLogicalOperatorsInWhenClause (MaxComplexity / 2 ) |> indent 8}
    let g() =
{makeMatchSnippetWithLogicalOperatorsInWhenClause (MaxComplexity / 2) |> indent 8}""" 
        this.Parse code
        Assert.IsTrue this.NoErrorsExist
        
    /// Verifies that the cyclomatic complexity is calculated on functions independently by checking that a function that comes after a function with a cyclomatic complexity that is flagged as too high need not be flagged.
    [<Test>]
    member this.EnsureComplexityOfFunctionsAreIndependent() =
        let code = $"""Module Program
let f() =
    {makeMatchSnippet (MaxComplexity + 1) |> indent 4}
let g() =
    {makeMatchSnippet MaxComplexity |> indent 4}"""
        this.Parse code
        Assert.AreEqual(1, this.ErrorRanges.Length)
        Assert.IsTrue(this.ErrorExistsAt(2, 4))
        
    /// Verifies that the cyclomatic complexity of nested functions are calculated independently by checking that evaluation of multiple nested functions each with a cyclomatic complexity equal to maxComplexity (so that the sum of their complexities is greater than maxComplexity) does not result in a flag.
    [<Test>]
    member this.EnsureComplexityOfNestedFunctionsWithLowComplexityAreIndependent() =
        let code = $"""Module Program 
let f() = 
    let g() =
{makeMatchSnippet MaxComplexity |> indent 8}
    let h() =
{makeMatchSnippet MaxComplexity |> indent 8}
    ()"""
        this.Parse code
        Assert.IsTrue this.NoErrorsExist
        
       
    /// Verifies that the cyclomatic complexity is calculated on nested functions independently by checking that a nested function that comes after another nested function with a cyclomatic complexity that is flagged as too high need not be flagged.
    [<Test>]
    member this.EnsureComplexityOfNestedFunctionsAreIndependent() =
        let code = $"""Module Program
let f() = 
    let g() =
        {(makeMatchSnippet (MaxComplexity+1)) |> indent 8} 
    let h() =
{makeMatchSnippet MaxComplexity |> indent 8} 
{makeMatchSnippet (MaxComplexity+1) |> indent 4}"""    
        this.Parse code
        Assert.AreEqual(2, this.ErrorRanges.Length)
        
    /// Verifies that the multiple messages are not provided for a single function.
    [<Test>]
    member this.EnsureRedundantWarningsNotReported() =
        // generates a vapid match clause
        let genMatchClause i = $"""| "{i}" -> match str with
    | "A" -> ()
    | "B" -> ()"""
        // create a snippet of code with 10 match clauses
        let matchClauses = [ 1..10 ] |> List.map genMatchClause |> List.map (indent 4) |> String.concat NewLine
        let code = """Module Program
let f (str: string) =
    match str with""" + NewLine + matchClauses
        this.Parse code
        Assert.AreEqual(1, this.ErrorRanges.Length)