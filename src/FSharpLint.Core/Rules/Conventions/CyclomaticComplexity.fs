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
            let leftTuple : ValueTuple<int, int, int> = (leftStart.Line, leftStart.Column, left.Complexity)
            let rightTuple : ValueTuple<int, int, int> = (rightStart.Line, rightStart.Column, right.Complexity)
            leftTuple.CompareTo rightTuple
    
///  A two-tiered stack-like structure for containing BindingScopes.
type private BindingStack(maxComplexity: int) =
    let mutable tier1 = List.Empty
    let mutable tier2 = SortedSet<BindingScope>(BindingScopeComparer())
    
    member this.Push (args:AstNodeRuleParams) (bs: BindingScope) =
        // if the node is a child of the current binding scope
        let isChildOfCurrent = 
            match tier1 with
            | [] ->
                false
            | head :: _ ->
                args.GetParents args.NodeIndex |> List.tryFind (fun astNode -> Object.ReferenceEquals(head.Node, astNode)) |> Option.isSome
        // if the node is not a child and the stack isn't empty, we're finished with the current head of tier1, so move it from tier1 to tier2
        match tier1 with
        | popped :: _ when not isChildOfCurrent ->
            tier1 <- tier1.Tail
            if popped.Complexity > maxComplexity then
                tier2.Add popped |> ignore<bool>
        | _ -> 
            ()
        // finally, push the item on to the stack
        tier1 <- bs::tier1
        
    member this.IncrComplexityOfCurrentScope incr =
        match tier1 with
        | head :: _ ->
            let complexity = head.Complexity + incr
            tier1 <- {head with Complexity = complexity}::tier1.Tail
        | [] -> 
            failwith $"{nameof(tier1)} is empty"
        
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
        tier1 <- List.Empty
        tier2.Clear()

/// A stack to track the current cyclomatic complexity of a binding scope.
let mutable private bindingStackOpt : BindingStack option = None
   
// recursive function to count the number of cases in a pattern.
[<TailCall>]
let rec private countCases (pat: SynPat) (count: int) =
    let localSoFar = count + 1
    match pat with
    | SynPat.Or (lhs, _, _, _) ->
        countCases lhs localSoFar
    | _ -> localSoFar

/// Boolean operator functions.
let private boolFunctions = Set.ofList ["op_BooleanOr"; "op_BooleanAnd"]

[<TailCall>]
let rec private countOperators count expressions =
    match expressions with
    | SynExpr.App(_, _, expr, SynExpr.Ident(ident), _) :: rest
    | SynExpr.App(_, _, SynExpr.Ident(ident), expr, _) :: rest ->
        if Set.contains ident.idText boolFunctions then
            countOperators (count + 1) (expr :: rest)
        else
            countOperators count (expr  :: rest)
    | SynExpr.App(_, _, expr, expr2, _) :: rest ->
        countOperators count (expr :: expr2 :: rest)
    | SynExpr.Paren(expr, _, _, _) :: rest ->
        countOperators count (expr :: rest)
    | ExpressionUtilities.Identifier([ ident ], _) :: rest ->
        if Set.contains ident.idText boolFunctions then
            countOperators (count + 1) rest
        else
            countOperators count rest
    // in match and match-like expressions, consider the when expressions of any clauses
    | SynExpr.MatchBang(_, _, clauses, _, _) :: rest
    | SynExpr.MatchLambda(_, _, clauses, _, _) :: rest
    | SynExpr.Match(_, _, clauses, _, _) :: rest ->
        let clauseExprs =
            clauses
            |> List.choose
                (fun matchClause ->
                    match matchClause with
                    | SynMatchClause(_, whenExprOpt, _, _, _, _) -> whenExprOpt)
        countOperators count (clauseExprs @ rest)
    | _ -> count

/// Runner for the rule. 
let runner (config:Config) (args:AstNodeRuleParams) : WarningDetails[] =
    /// gets the global binding stack 
    let getBindingStack (maxComplexity: int) =
        match bindingStackOpt with
        | Some bs -> bs
        | None -> let bs = BindingStack maxComplexity
                  bindingStackOpt <- Some bs
                  bs
    
    /// Determines the number of cases in a match clause.
    let countCasesInMatchClause (clause: SynMatchClause) =
        match clause with 
        | SynMatchClause(pat, _, _, _, _, _) -> countCases pat 0
    
    /// Returns the number of boolean operators in an expression.
    /// If expression is Match, MatchLambda, or MatchBang, the 'when' expressions of the match clauses are examined for boolean operators, if applicable.
    let countBooleanOperators expression =
        countOperators 0 (List.singleton expression)

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
            | SynExpr.MatchBang(_, _, clauses, _, _)
            | SynExpr.MatchLambda(_, _, clauses, _, _)
            | SynExpr.Match(_, _, clauses, _, _) ->
                let numCases = List.sumBy countCasesInMatchClause clauses // determine the number of cases in the match expression 
                bindingStack.IncrComplexityOfCurrentScope (numCases + countBooleanOperators expression) // include the number of boolean operators in any when expressions, if applicable
            | _ -> ()
        | _ -> ()
    
    // if the last node to be processed, pop everything off the stack
    if args.NodeIndex >= args.SyntaxArray.Length-1 then
            let fromStack = bindingStack
                            |> Seq.sortBy (fun scope -> // sort by order of start position, for reporting
                                 let pos = scope.Binding.RangeOfBindingWithRhs.Start
                                 (pos.Column, pos.Line))
                            |> Seq.map (fun scope -> // transform into WarningDetails
                                let errMsg = String.Format(Resources.GetString("RulesCyclomaticComplexityError"), scope.Complexity, config.MaxComplexity)
                                { Range = scope.Binding.RangeOfBindingWithRhs; Message = errMsg; SuggestedFix = None; TypeChecks = List.Empty })
                            |> Seq.toList
            let ret = match warningDetails with
                      | Some warning -> warning::fromStack
                      | None -> fromStack
            List.toArray ret
    else
        Array.empty
  
/// Resets call stack after a call to runner.
let cleanup () =
    match bindingStackOpt with
    | Some bs -> bs.Clear()
    | None -> ()

/// Generator function for a rule instance.
let rule config =
    AstNodeRule
        {
            Name = "CyclomaticComplexity"
            Identifier = Identifiers.CyclomaticComplexity
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = cleanup
                }
        }
