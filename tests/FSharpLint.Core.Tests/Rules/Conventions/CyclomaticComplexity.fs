module FSharpLint.Core.Tests.Rules.Conventions.CyclomaticComplexity

open NUnit.Framework
open FSharpLint.Rules.CyclomaticComplexity
open System

/// The max cyclomatic complexity as configured in these tests.
[<Literal>]
let private maxComplexity = 5

let private makeMatchSnippet indent newline len =
    indent + "match \"dummyString\" with" + newline +  
        (Seq.map (fun i -> (sprintf "%s| \"%d\" -> ()" indent i)) [| 1..len-1 |] |> String.concat newline) + newline + 
        indent + "| _ -> ()"
        
/// Generates a body of code containing a match expression with a when clause containing a logical operator in each pattern.
let private makeMatchSnippetWithLogicalOperatorsInWhenClause indent newline (len: int) =
        indent + "match \"dummyString\" with" + newline +  
        (Seq.map (fun i -> (sprintf "%s| x when x = \"%d\" || x = \"%d\" -> ()" indent (i*len) (i*len+1))) [| 1..len-1 |] |> String.concat newline) + newline + 
        indent + "| _ -> ()"

/// module declaration and let binding declaration for a body of code
let private makeProgram funcString newline body =
    "Module Program" + newline
    + "let " + funcString + " =" + newline
    + body

/// Generates a body of code consisting of if-else expressions.
let private ifElseExpressions indent newline len =
    indent + "if true then ()" + 
        String.replicate (len-1) (sprintf "%s%selif true then ()" newline indent)
    |> makeProgram "f()" newline 

/// Generates a body of code containing for expressions.
let private forExpressions indent newline len =
    String.replicate len (sprintf "%sfor i = 0 to 1 do ()%s" indent newline)
    |> makeProgram "f()" newline 
  
/// Generates a body of code containing foreach expressions.    
let private foreachExpressions indent newline len =
    String.replicate len (sprintf "%sfor _ in [] do ()%s" indent newline)
    |> makeProgram "f()" newline 

/// Generates a body of code containing while expressions.   
let private whileExpressions indent newline len =
    String.replicate len (sprintf "%swhile false do ()%s" indent newline)
    |> makeProgram "f()" newline 

/// Generates a body of code containing a while expression with multiple logical operators in the condition.
let private whileWithBooleanOperatorsInConditionExpressions indent newline len =
    if len < 2 then invalidArg (nameof len) "must be > 2"
    indent + "while true &&" + 
    String.replicate (len-2) (sprintf "%s%sfalse &&" newline indent) + newline + 
    indent + "true do ()"
    |> makeProgram "f()" newline 

/// Generates a body of code containing an if statement with multiple && conditional operators 
let private ifThenExpressionWithMultipleAndConditionals indent newline len =
    if len < 2 then invalidArg (nameof len) "must be > 2"
    indent + "if true &&" + 
    String.replicate (len-2) (sprintf "%s%sfalse &&" newline indent) + newline + 
    indent + "true then ()"
    |> makeProgram "f()" newline 

/// Generates a body of code containing an if statement with multiple || conditional operators    
let private ifThenExpressionWithMultipleOrConditionals indent newline len =
    if len < 2 then invalidArg (nameof len) "must be > 2"
    indent + "if true ||" + 
    String.replicate (len-2) (sprintf "%s%sfalse ||" newline indent) + newline + 
    indent + "true then ()"
    |> makeProgram "f()" newline 
   
/// Generates a body of code containing a match expression with multiple patterns.
let private matchExpression indent newline len =
    makeMatchSnippet indent newline len
    |> makeProgram "f()" newline 
    
/// Generates a body of code containing a match expression with multiple combined patterns.
let private matchExpressionWithCombinedPatterns indent newline len =
    indent + "match \"dummyString\" with" + newline +  
        (Seq.map (fun i -> (sprintf "%s| \"%d\"" indent i)) [| 1..len-1 |] |> String.concat newline) + newline + 
        indent + "| _ -> ()"
        |> makeProgram "f()" newline 

/// Generates a body of code containing a match function with multiple patterns.
let private matchFunction indent newline len =
    indent + "function" + newline + 
    (Seq.map (fun i -> (sprintf "%s| \"%d\"" indent i)) [| 1..len-1 |] |> String.concat newline) + newline + 
    indent + "| _ -> ()" + newline +
    indent + "f \"dummyString\""
    |> makeProgram "f" newline
   
/// Generates a computational expression with a match! expression containing multiple patterns.
let private matchBang indent newline len =
    sprintf "%sasync {" indent + newline +
    sprintf "%smatch! async {return \"dummyString\" } with" indent + newline +
    (Seq.map (fun i -> (sprintf "%s| \"%d\"" indent i)) [| 1..len-1 |] |> String.concat newline) + newline + 
    sprintf "%s| _ -> ()" indent
    |> makeProgram "a" newline
     
/// Tests for the cyclomatic complexity rule.
type TestConventionsCyclomaticComplexity() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(rule { Config.MaxComplexity = maxComplexity; })
 
    /// indentation for source code snippets used to test rule.
    static member private Indent = "    "
    static member private NewLine = Environment.NewLine

    /// Generator for source code test cases which have cyclomatic complexity equal to maxComplexity.
    /// Yields an enumerable of source code snippets.
    static member private AtMaxComplexityTestCasesSource =
        seq {
            let ind = TestConventionsCyclomaticComplexity.Indent
            let nl = TestConventionsCyclomaticComplexity.NewLine
            let num = maxComplexity
            yield ifElseExpressions ind nl num
            yield forExpressions ind nl num
            yield foreachExpressions ind nl num
            yield whileExpressions ind nl num
            yield matchExpression ind nl num
            yield matchExpressionWithCombinedPatterns ind nl num
            yield matchFunction ind nl num
            yield matchBang ind nl num  
            yield ifThenExpressionWithMultipleAndConditionals ind nl num
            yield ifThenExpressionWithMultipleOrConditionals ind nl num
            yield whileWithBooleanOperatorsInConditionExpressions ind nl num
        } |> Seq.map box |> Seq.map Array.singleton // return an enumerable of obj arrays
    
    /// Generator for source code test cases which have cyclomatic complexity greater than maxComplexity.
    /// Yields an enumerable of (source code snippet, errorLocation) pairs.
    static member private FailureCasesSource =
        seq {
            let ind = TestConventionsCyclomaticComplexity.Indent
            let nl = TestConventionsCyclomaticComplexity.NewLine
            let num = maxComplexity + 1
            
            /// basic conditional expressions
            let errorLocation = maxComplexity+3, TestConventionsCyclomaticComplexity.Indent.Length
            yield ifElseExpressions ind nl num, errorLocation
            yield forExpressions ind  nl num, errorLocation
            yield foreachExpressions ind  nl num, errorLocation
            yield whileExpressions ind nl num, errorLocation

            /// match and function expressions
            let errorLocation = 3, TestConventionsCyclomaticComplexity.Indent.Length
            yield matchExpression ind nl num, errorLocation
            yield matchExpressionWithCombinedPatterns ind nl num, errorLocation
            yield matchFunction ind nl num, errorLocation
            
            /// match! expression
            let errorLocation = 4, TestConventionsCyclomaticComplexity.Indent.Length
            yield matchBang ind nl num, errorLocation
            
            /// boolean conditionals
            let errorLocation = 3, TestConventionsCyclomaticComplexity.Indent.Length
            yield ifThenExpressionWithMultipleAndConditionals  ind nl num, errorLocation
            yield ifThenExpressionWithMultipleOrConditionals ind nl num, errorLocation
            yield whileWithBooleanOperatorsInConditionExpressions ind nl num, errorLocation    
        } |> Seq.map (fun (x, y) -> [| box x; box y |])
 
    /// Verifies that no cyclomatic complexity over-maximum flags are raised on source that has cyclomatic complexity <= maxComplexity.
    [<TestCaseSource(nameof(TestConventionsCyclomaticComplexity.AtMaxComplexityTestCasesSource))>]
    member this.EnsureNoErrorWhenComplexityBelowThreshold(code) =
        this.Parse code
        Assert.IsFalse(this.ErrorsExist)
      
    /// Verifies that flags are raised on source code that has cyclomatic complexity > maxComplexity.
    [<TestCaseSource(nameof(TestConventionsCyclomaticComplexity.FailureCasesSource))>]
    member this.EnsureErrorWhenComplexityExceedsThreshold(code, errorLocation) =
        this.Parse code
        Assert.IsTrue(this.ErrorExistsAt(errorLocation))
    
    /// Verifies that an error is raised on a match expression which has multiple boolean operator conditions in multiple when clauses.
    [<Test>]
    member this.TestLogicalOperatorsInMatch() =
        let indent = TestConventionsCyclomaticComplexity.Indent
        let newline = TestConventionsCyclomaticComplexity.NewLine
        let code = "Module Program" + newline + 
                   "let f() = " + newline +
                   makeMatchSnippetWithLogicalOperatorsInWhenClause indent newline (maxComplexity + 1) 
        this.Parse code
        Assert.AreEqual(1, this.ErrorRanges.Length) 
    
    /// Verifies that an error is raised on a single match clause with multiple logical operators.
    [<Test>]
    member this.TestMultipleLogicalOperatorsInSingleMatchClause() =
        let indent = TestConventionsCyclomaticComplexity.Indent
        let newline = TestConventionsCyclomaticComplexity.NewLine
        let code = "Module Program" + newline + 
                   "let f() = " + newline +
                   indent + "match \"dummyString\" with" + newline +
                   indent + "| x when x = \"1\"" + ([2..maxComplexity+1] |> List.map (sprintf "|| x = \"%d\"") |> String.concat " ") + " -> ()" + newline +
                   indent + "| _ -> ()"
        this.Parse code
        Assert.AreEqual(1, this.ErrorRanges.Length)
      
    /// Verifies that no cyclomatic complexity error is flagged when boolean operators are raised outside of conditional branching contexts (sanity check).
    [<Test>]
    member this.EnsureNoErrorWhenLogicalExpressionOutsideCondition() =
        let indent = TestConventionsCyclomaticComplexity.Indent
        let newline = TestConventionsCyclomaticComplexity.NewLine
        let code = "Module Program" + newline +
                   "let f() =" + newline +
                   indent + "let t = true " + String.replicate (maxComplexity+1) "|| false "  + newline +
                   indent + "()" 
        this.Parse code
        Assert.IsTrue(this.NoErrorsExist)
    
    /// Verifies that the cyclomatic complexity of functions is independent, by checking that a snippet of code with two functions, each with a complexity lower than the threshold but with a sum complexity greater than the threshold, does not flag an error. 
    [<Test>]
    member this.EnsureComplexityOfFunctionsWithLowComplexityIsIndependent() =
        let newline = TestConventionsCyclomaticComplexity.NewLine
        let indent = TestConventionsCyclomaticComplexity.Indent
        let code = "Module Program" + newline +
                   "let f() = " + newline + makeMatchSnippetWithLogicalOperatorsInWhenClause indent newline (maxComplexity / 2 ) + newline + 
                   "let g() = " + newline + makeMatchSnippetWithLogicalOperatorsInWhenClause indent newline (maxComplexity / 2)   
        this.Parse code
        Assert.AreEqual(0, this.ErrorRanges.Length)
        
    /// Verifies that the cyclomatic complexity is calculated on functions independently by checking that a function that comes after a function with a cyclomatic complexity that is flagged as too high need not be flagged.
    [<Test>]
    member this.EnsureComplexityOfFunctionsIsIndependent() =
        let newline = TestConventionsCyclomaticComplexity.NewLine
        let indent = TestConventionsCyclomaticComplexity.Indent
        let code = "Module Program" + newline +
                   "let f() =" + newline + makeMatchSnippet indent newline (maxComplexity + 1) + newline + 
                   "let g() =" + newline + makeMatchSnippet indent newline maxComplexity 
        this.Parse code
        Assert.AreEqual(1, this.ErrorRanges.Length)
        Assert.IsTrue(this.ErrorExistsAt(3, indent.Length))
        
    /// Verifies that the cyclomatic complexity of nested functions are calculated independently by checking that evaluation of multiple nested functions each with a cyclomatic complexity equal to maxComplexity (so that the sum of their complexities is greater than maxComplexity) does not result in a flag.
    [<Test>]
    member this.EnsureComplexityOfNestedFunctionsWithLowComplexityIsIndependent() =
        let newline = TestConventionsCyclomaticComplexity.NewLine
        let indent = TestConventionsCyclomaticComplexity.Indent
        let code = "Module Program" + newline + 
                        "let f() = " + newline +
                        indent + "let g() = " + newline + makeMatchSnippet (indent+indent) newline maxComplexity + newline +
                        indent + "let g() = " + newline + makeMatchSnippet (indent+indent) newline maxComplexity + newline +
                        makeMatchSnippet indent newline maxComplexity   
        this.Parse code
        Assert.AreEqual(0, this.ErrorRanges.Length)
        
       
    /// Verifies that the cyclomatic complexity is calculated on nested functions independently by checking that a nested function that comes after another nested function with a cyclomatic complexity that is flagged as too high need not be flagged.
    [<Test>]
    member this.EnsureComplexityOfNestedFunctionsIsIndependent() =
        let newline = TestConventionsCyclomaticComplexity.NewLine
        let indent = TestConventionsCyclomaticComplexity.Indent
        let code = "Module Program" + newline + 
                        "let f() = " + newline +
                        indent + "let g() = " + newline + makeMatchSnippet (indent+indent) newline (maxComplexity+1) + newline +
                        indent + "let g() = " + newline + makeMatchSnippet (indent+indent) newline maxComplexity + newline +
                        makeMatchSnippet indent newline (maxComplexity+1)   
        this.Parse code
        Assert.AreEqual(2, this.ErrorRanges.Length)
        