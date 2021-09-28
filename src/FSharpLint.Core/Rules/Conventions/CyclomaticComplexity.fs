module FSharpLint.Rules.CyclomaticComplexity
    
open System
open System.IO
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

/// Configuration of the cyclomatic complexity (FL0069) rule.
[<RequireQualifiedAccess>]
type Config = {
    /// The maximum cyclomatic complexity for a function or method before an issue is flagged.
    MaxComplexity: int
}

/// The scope of a binding. 
type BindingScope =
    {
       /// The Node corresponding to the binding.
       Node: AstNode
       /// the cyclomatic complexity of the binding scope.
       Complexity: int
    }

/// A stack to track the current cyclomatic complexity of a binding scope.
let mutable private BindingStack : BindingScope list = []
   
/// Determines the number of cases in a match clause.
let private countCasesInMatchClause (clause: SynMatchClause) =
    // recursive function to count the number of cases in a pattern.
    let rec countCases (pat: SynPat) (count: int) =
        let mutable localSoFar = count + 1
        match pat with
        | SynPat.Or (lhs, _, _) ->
            countCases lhs localSoFar
        | _ -> localSoFar
    // apply countCases to the given clause.
    match clause with 
    | SynMatchClause(pat, _, _, _, _) -> countCases pat 0

let private increaseComplexity (config: Config) (expression: SynExpr) incr =
    let h = BindingStack.Head
    let complexity = h.Complexity + incr
    BindingStack <- {h with Complexity = complexity}::BindingStack
    if complexity > config.MaxComplexity then
        let errorFormatString = Resources.GetString("RulesCyclomaticComplexityError")
        let errMsg = String.Format(errorFormatString, config.MaxComplexity)
        [| { Range = expression.Range; Message = errMsg; SuggestedFix = None; TypeChecks = [] } |]
    else
        Array.empty
    

/// Boolean operator functions.
let private boolFunctions = Set.ofList ["op_BooleanOr"; "op_BooleanAnd"]

/// Returns the number of boolean operators in an expression.
/// If expression is Match, MatchLambda, or MatchBang, the 'when' expressions of the match clauses are examined for boolean operators, if applicable.
let private countBooleanOperators expression =
    let rec countOperators count = function
    | SynExpr.App(_, _, expr, SynExpr.Ident(ident), _)
    | SynExpr.App(_, _, SynExpr.Ident(ident), expr, _) ->
        if Set.contains ident.idText boolFunctions then
            countOperators (count + 1) expr
        else
            countOperators count expr
    | SynExpr.App(_, _, expr, expr2, _) ->
        let left = countOperators 0 expr
        let right = countOperators 0 expr2
        count + left + right
    | SynExpr.Paren(expr, _, _, _) ->
        countOperators count expr
    | SynExpr.Ident ident ->
        if Set.contains ident.idText boolFunctions then
            count + 1
        else
            count
    // in match and match-like expressions, consider the when expressions of any clauses
    | SynExpr.MatchBang(_, _, clauses, _)
    | SynExpr.MatchLambda(_, _, clauses, _, _) 
    | SynExpr.Match(_, _, clauses, _) ->
        clauses |> List.sumBy (fun c -> 
                                      match c with
                                      | SynMatchClause(_, whenExprOpt, _, _, _) ->
                                          match whenExprOpt with
                                          | Some whenExpr ->
                                              countOperators 0 whenExpr
                                          | None -> 0)
               
    | _ -> count
    // kick off the calculation
    countOperators 0 expression

/// Runner for the rule. 
let runner (config:Config) (args:AstNodeRuleParams) : WarningDetails[] =   
    let node = args.AstNode
    let parentIndex = args.SyntaxArray.[args.NodeIndex].ParentIndex
    // determine if the node is a duplicate of a node in the AST containining ExtraSyntaxInfo (e.g. lambda arg being a duplicate of the lambda itself)
    let isMetaData = if parentIndex = args.NodeIndex then
                         false
                     else
                         Object.ReferenceEquals(node, args.SyntaxArray.[parentIndex].Actual)
    // determine if the node is a binding, and so will be pushed onto the stack
    let isBinding = match node with
                     | AstNode.Binding _ -> true 
                     | _ -> false
    // if the node is a child of the current binding scope
    let isChildOfCurrent = if List.isEmpty BindingStack then
                                   false
                               else
                                   args.GetParents args.NodeIndex |> List.tryFind (fun x -> Object.ReferenceEquals(BindingStack.Head.Node, x)) |> Option.isSome
    // if the node is not a child and the stack isn't empty, we're finished with the current binding scope at the head of the stack
    if not isChildOfCurrent && List.length BindingStack > 0 then
        BindingStack <- BindingStack.Tail
    // if the node is a binding, pop it onto the stack
    if isBinding then
        BindingStack <- { Node = node; Complexity = 0 }::BindingStack
         
    // if not metadata, match the node against an expression which increments the complexity
    if not isMetaData then
        match node with
        | AstNode.Expression expression ->
            match expression with
            | SynExpr.For _ ->
                increaseComplexity config expression 1
            | SynExpr.ForEach _ ->
                increaseComplexity config expression 1
            | SynExpr.While(_, condition, _, _) ->
                increaseComplexity config expression (1 +  + countBooleanOperators condition) // include the number of boolean operators in the while condition
            | SynExpr.IfThenElse(condition, _, _, _, _, _, _) ->
                 increaseComplexity config expression (1 + countBooleanOperators condition) // include the number of boolean operators in the condition
            | SynExpr.MatchBang(_, _, clauses, _)
            | SynExpr.MatchLambda(_, _, clauses, _, _)
            | SynExpr.Match(_, _, clauses, _) ->
                let numCases = clauses |> List.sumBy countCasesInMatchClause // determine the number of cases in the match expression 
                increaseComplexity config expression (numCases + countBooleanOperators expression) // include the number of boolean operators in any when expressions, if applicable
            | _ -> Array.empty 
        | _ -> Array.empty
    else
        Array.empty
  
/// Resets call stack after a call to runner.
let cleanup () =
    BindingStack <- []

/// Generator function for a rule instance.
let rule config =
    { Name = "CyclomaticComplexity"
      Identifier = Identifiers.CyclomaticComplexity
      RuleConfig = { AstNodeRuleConfig.Runner = runner config
                     Cleanup = cleanup } }
    |> AstNodeRule