module FSharpLint.Rules.CyclomaticComplexity
    
open System
open System.Collections.Generic
open System.Linq
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
type private BindingScope =
    {
       /// The Node corresponding to the binding.
       Node: AstNode
       /// The binding of the node
       Binding: SynBinding
       /// the cyclomatic complexity of the binding scope.
       Complexity: int
    }

type private BindingScopeComparer() =
    interface IComparer<BindingScope> with
        member this.Compare(left, right) =
            let leftStart = left.Binding.RangeOfBindingWithoutRhs.Start
            let rightStart = right.Binding.RangeOfBindingWithoutRhs.Start
            let leftTuple : ValueTuple<int, int, int> = leftStart.Line, leftStart.Column, left.Complexity
            let rightTuple : ValueTuple<int, int, int> = rightStart.Line, rightStart.Column, right.Complexity
            leftTuple.CompareTo rightTuple
    
///  A two-tiered stack-like structure for containing BindingScopes.
type private BindingStack(maxComplexity: int) =
    let mutable tier1 = []
    let mutable tier2 = SortedSet<BindingScope>(BindingScopeComparer())
    
    member this.Push (args:AstNodeRuleParams) (bs: BindingScope) =
        // if the node is a child of the current binding scope
        let isChildOfCurrent = if List.isEmpty tier1 then
                                    false
                                else
                                    args.GetParents args.NodeIndex |> List.tryFind (fun x -> Object.ReferenceEquals(tier1.Head.Node, x)) |> Option.isSome
        // if the node is not a child and the stack isn't empty, we're finished with the current head of tier1, so move it from tier1 to tier2
        if not isChildOfCurrent && not (List.isEmpty tier1) then
            let popped = tier1.Head
            tier1 <- tier1.Tail
            if popped.Complexity > maxComplexity then
                tier2.Add popped |> ignore
        // finally, push the item on to the stack
        tier1 <- bs::tier1
        
    member this.IncrComplexityOfCurrentScope incr =
        let h = tier1.Head
        let complexity = h.Complexity + incr
        tier1 <- {h with Complexity = complexity}::tier1.Tail
        
    interface IEnumerable<BindingScope> with
        member this.GetEnumerator() =
            let cleanedUp =
                tier1
                |> List.filter (fun scope -> scope.Complexity > maxComplexity)
                |> List.sortByDescending (fun scope -> scope.Complexity) // sort in descending order by complexity
                |> List.distinctBy (fun scope -> scope.Binding.RangeOfBindingWithRhs.Start) // throw away any extraneous elements with the same start position but a lower complexity
            let enum1 = cleanedUp :> IEnumerable<BindingScope>
            let enum2 = tier2 :> IEnumerable<BindingScope>
            Enumerable.Concat(enum1, enum2).GetEnumerator()
            
        member this.GetEnumerator(): Collections.IEnumerator = (this :> IEnumerable<BindingScope> :> System.Collections.IEnumerable).GetEnumerator()
        
    /// Clears the stack.
    member this.Clear() =
        tier1 <- []
        tier2.Clear()

/// A stack to track the current cyclomatic complexity of a binding scope.
let mutable private bindingStackOpt : BindingStack option = None

/// gets the global binding stack 
let private getBindingStack (maxComplexity: int) =
    match bindingStackOpt with
    | Some bs -> bs
    | None -> let bs = BindingStack maxComplexity
              bindingStackOpt <- Some bs
              bs
   
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
    let bindingStack = getBindingStack config.MaxComplexity
    
    let mutable warningDetails = None
    let node = args.AstNode
    let parentIndex = args.SyntaxArray.[args.NodeIndex].ParentIndex
    // determine if the node is a duplicate of a node in the AST containing ExtraSyntaxInfo (e.g. lambda arg being a duplicate of the lambda itself)
    let isMetaData = if parentIndex = args.NodeIndex then
                         false
                     else
                         Object.ReferenceEquals(node, args.SyntaxArray.[parentIndex].Actual)
    // determine if the node is a binding, and so will be pushed onto the stack
    let bindingOpt = match node with
                     | AstNode.Binding binding -> Some binding
                     | _ -> None
    
    // if the node is a binding, push it onto the stack
    match bindingOpt with
    | Some binding -> bindingStack.Push args { Node = node; Binding = binding; Complexity = 0 }
    | None -> ()
         
    // if not metadata, match the node against an expression which increments the complexity
    if not isMetaData then
        match node with
        | AstNode.Expression expression ->
            match expression with
            | SynExpr.For _ ->
                bindingStack.IncrComplexityOfCurrentScope 1
            | SynExpr.ForEach _ ->
                bindingStack.IncrComplexityOfCurrentScope 1
            | SynExpr.While(_, condition, _, _) ->
                bindingStack.IncrComplexityOfCurrentScope (1 + countBooleanOperators condition) // include the number of boolean operators in the while condition
            | SynExpr.IfThenElse(condition, _, _, _, _, _, _) ->
                 bindingStack.IncrComplexityOfCurrentScope (1 + countBooleanOperators condition) // include the number of boolean operators in the condition
            | SynExpr.MatchBang(_, _, clauses, _)
            | SynExpr.MatchLambda(_, _, clauses, _, _)
            | SynExpr.Match(_, _, clauses, _) ->
                let numCases = clauses |> List.sumBy countCasesInMatchClause // determine the number of cases in the match expression 
                bindingStack.IncrComplexityOfCurrentScope (numCases + countBooleanOperators expression) // include the number of boolean operators in any when expressions, if applicable
            | _ -> ()
        | _ -> ()
    
    // if the last node to be processed, pop everything off the stack
    if args.NodeIndex >= args.SyntaxArray.Length-1 then
            let fromStack = bindingStack
                            |> Seq.sortBy (fun scope -> // sort by order of start position, for reporting
                                 let pos = scope.Binding.RangeOfBindingWithRhs.Start
                                 pos.Column, pos.Line)
                            |> Seq.map (fun scope -> // transform into WarningDetails
                                let errMsg = String.Format(Resources.GetString("RulesCyclomaticComplexityError"), scope.Complexity, config.MaxComplexity)
                                { Range = scope.Binding.RangeOfBindingWithRhs; Message = errMsg; SuggestedFix = None; TypeChecks = [] })
                            |> Seq.toList
            let ret = match warningDetails with
                      | Some x -> x::fromStack
                      | None -> fromStack
            ret |> List.toArray
    else
        Array.empty
  
/// Resets call stack after a call to runner.
let cleanup () =
    match bindingStackOpt with
    | Some bs -> bs.Clear()
    | None -> ()

/// Generator function for a rule instance.
let rule config =
    { Name = "CyclomaticComplexity"
      Identifier = Identifiers.CyclomaticComplexity
      RuleConfig = { AstNodeRuleConfig.Runner = runner config
                     Cleanup = cleanup } }
    |> AstNodeRule