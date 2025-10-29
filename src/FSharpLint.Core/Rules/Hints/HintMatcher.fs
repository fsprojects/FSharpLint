module FSharpLint.Rules.HintMatcher

open System
open System.Collections.Generic
open System.Diagnostics
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.ExpressionUtilities
open FSharpLint.Framework.HintParser
open FSharpLint.Framework.Rules
open FSharpLint.Framework.HintParserTypes

type ToStringConfig =
    {
        Replace: bool
        ParentAstNode: AstNode option
        Args: AstNodeRuleParams
        MatchedVariables: Dictionary<char, SynExpr>
        ParentHintNode: option<HintNode>
        HintNode: HintNode
    }

type Config =
    { HintTrie:MergeSyntaxTrees.Edges }

let private extractSimplePatterns (SynSimplePats.SimplePats(simplePatterns, _, _)) =
    simplePatterns

let rec private extractIdent = function
    | SynSimplePat.Id(ident, _, isCompilerGenerated, _, _, _) -> (ident, isCompilerGenerated)
    | SynSimplePat.Attrib(simplePattern, _, _)
    | SynSimplePat.Typed(simplePattern, _, _) -> extractIdent simplePattern

[<RequireQualifiedAccess>]
type private LambdaArgumentMatch =
    | Variable of variable:char * identifier:string
    | Wildcard
    | NoMatch

let private matchLambdaArgument (LambdaArg.LambdaArg(hintArg), actualArg) =
    match extractSimplePatterns actualArg with
    | [] -> LambdaArgumentMatch.NoMatch
    | simplePattern::_ ->
        let identifier, isCompilerGenerated = extractIdent simplePattern

        let isWildcard = isCompilerGenerated && identifier.idText.StartsWith("_")

        match hintArg with
        | Expression.LambdaArg(Expression.Variable(variable)) when not isWildcard ->
            LambdaArgumentMatch.Variable(variable, identifier.idText)
        | Expression.LambdaArg(Expression.Wildcard) -> LambdaArgumentMatch.Wildcard
        | _ -> LambdaArgumentMatch.NoMatch

[<RequireQualifiedAccess>]
type private LambdaMatch =
    | Match of Map<char, string>
    | NoMatch

let private matchLambdaArguments (hintArgs:HintParserTypes.LambdaArg list) (actualArgs:SynSimplePats list) =
    if List.length hintArgs <> List.length actualArgs then
        LambdaMatch.NoMatch
    else
        let matches =
            List.zip hintArgs actualArgs
            |> List.map matchLambdaArgument

        let allArgsMatch =
            List.forall (function
                | LambdaArgumentMatch.NoMatch -> false
                | _ -> true) matches

        if allArgsMatch then
            matches
            |> List.choose (function
                | LambdaArgumentMatch.Variable(variable, ident) -> Some(variable, ident)
                | _ -> None)
            |> Map.ofList
            |> LambdaMatch.Match
        else
            LambdaMatch.NoMatch

/// Converts a SynConst (FSharp AST) into a Constant (hint AST).
let private matchConst = function
    | SynConst.Bool value -> Some(Constant.Bool value)
    | SynConst.Int16 value  -> Some(Constant.Int16 value)
    | SynConst.Int32 value  -> Some(Constant.Int32 value)
    | SynConst.Int64 value  -> Some(Constant.Int64 value)
    | SynConst.UInt16 value  -> Some(Constant.UInt16 value)
    | SynConst.UInt32 value -> Some(Constant.UInt32 value)
    | SynConst.UInt64 value  -> Some(Constant.UInt64 value)
    | SynConst.Byte value  -> Some(Constant.Byte value)
    | SynConst.Bytes (value, _, _)  -> Some(Constant.Bytes value)
    | SynConst.Char value  -> Some(Constant.Char value)
    | SynConst.Decimal value  -> Some(Constant.Decimal value)
    | SynConst.Double value  -> Some(Constant.Double value)
    | SynConst.SByte value  -> Some(Constant.SByte value)
    | SynConst.Single value  -> Some(Constant.Single value)
    | SynConst.String (value, _, _)  -> Some(Constant.String value)
    | SynConst.UIntPtr value  -> Some(Constant.UIntPtr(unativeint value))
    | SynConst.IntPtr value  -> Some(Constant.IntPtr(nativeint value))
    | SynConst.UserNum (value, endChar)  ->
        Some(Constant.UserNum(System.Numerics.BigInteger.Parse(value), endChar.[0]))
    | SynConst.Unit -> Some Constant.Unit 
    | SynConst.UInt16s _ 
    | SynConst.SourceIdentifier _
    | SynConst.Measure _  -> None

module private Precedence =
    let private ofHint hint =
        match hint with
        | HintExpr(expr) ->
            match expr with
            | Expression.Lambda(_) -> Some 3
            | Expression.If(_) -> Some 2
            | Expression.AddressOf(_) | Expression.PrefixOperator(_) | Expression.InfixOperator(_)
            | Expression.FunctionApplication(_) -> Some 1
            | _ -> None
        | HintPat(_) -> None

    let private ofExpr expr =
        match expr with
        | SynExpr.Lambda(_) | SynExpr.MatchLambda(_) | SynExpr.Match(_)
        | SynExpr.TryFinally(_) | SynExpr.TryWith(_) -> Some 3
        | SynExpr.IfThenElse(_) -> Some 2
        | SynExpr.InferredDowncast(_) | SynExpr.InferredUpcast(_) | SynExpr.Assert(_) | SynExpr.Fixed(_)
        | SynExpr.Lazy(_) | SynExpr.New(_) | SynExpr.Tuple(true (* true = struct tuple  *) , _, _, _)
        | SynExpr.Downcast(_) | SynExpr.Upcast(_) | SynExpr.TypeTest(_) | SynExpr.AddressOf(_)
        | SynExpr.App(_) -> Some 1
        | _ -> None

    let requiresParenthesis (matchedVariables:Dictionary<_, _>) hintNode parentAstNode parentHintNode =
        let parentPrecedence =
            match parentHintNode with
            | Some(hint) -> ofHint hint
            | None ->
                match parentAstNode with
                | Some(AstNode.Expression(expr)) -> ofExpr expr
                | Some(_) | None -> None

        let hintPrecedence =
            match hintNode with
            | HintExpr(Expression.Variable(varChar)) ->
                match matchedVariables.TryGetValue varChar with
                | true, expr -> ofExpr expr
                | _ -> None
            | hint -> ofHint hint

        match (hintPrecedence, parentPrecedence) with
        | Some hint, Some parent -> hint >= parent
        | _ -> false

let private filterParens astNodes =
    let isNotParen = function
        | AstNode.Expression(SynExpr.Paren(_)) -> false
        | _ -> true

    List.filter isNotParen astNodes

module private MatchExpression =

    /// Extracts an expression from parentheses e.g. ((x + 4)) -> x + 4
    let rec private removeParens = function
        | AstNode.Expression(SynExpr.Paren(expr, _, _, _)) -> expr |> AstNode.Expression |> removeParens
        | node -> node

    [<NoEquality; NoComparison>]
    type Arguments =
        { LambdaArguments:Map<char, string>
          MatchedVariables:Dictionary<char, SynExpr>
          Expression:AstNode
          Hint:Expression
          FSharpCheckFileResults:FSharpCheckFileResults option
          Breadcrumbs:AstNode list }

        with
            member this.SubHint(expr, hint) =
                { this with Expression = expr; Hint = hint }

    let private matchExpr = function
        | AstNode.Expression(ExpressionUtilities.Identifier([ident], _)) ->
            let ident = identAsDecompiledOpName ident
            Some(Expression.Identifier([ident]))
        | AstNode.Expression(SynExpr.LongIdent(_, ident, _, _)) ->
            let identifier = List.map (fun (ident: Ident) -> ident.idText) ident.LongIdent
            Some(Expression.Identifier(identifier))
        | AstNode.Expression(SynExpr.Const(constant, _)) ->
            matchConst constant |> Option.map Expression.Constant
        | AstNode.Expression(SynExpr.Null(_)) ->
            Some(Expression.Null)
        | _ -> None

    let private (|PossiblyInMethod|PossiblyInConstructor|NotInMethod|) breadcrumbs =
        let (|PossiblyMethodCallOrConstructor|_|) = function
            | SynExpr.App(_, false, _, _, _) -> Some()
            | _ -> None

        match breadcrumbs with
        | AstNode.Expression(SynExpr.Tuple(_))::AstNode.TypeParameter(_)::AstNode.Expression(SynExpr.New(_))::_
        | AstNode.Expression(SynExpr.Tuple(_))::AstNode.Expression(SynExpr.New(_))::_
        | AstNode.TypeParameter(_)::AstNode.Expression(SynExpr.New(_))::_
        | AstNode.Expression(SynExpr.New(_))::_ ->
            PossiblyInConstructor
        | AstNode.Expression(PossiblyMethodCallOrConstructor)::_
        | AstNode.Expression(SynExpr.Tuple(_))::AstNode.Expression(PossiblyMethodCallOrConstructor)::_
        | AstNode.Expression(SynExpr.Tuple(_))::AstNode.TypeParameter(_)::AstNode.Expression(PossiblyMethodCallOrConstructor)::_ ->
            PossiblyInMethod
        | _ -> NotInMethod

    [<NoComparison; NoEquality>]
    type HintMatch =
        | Match of (unit -> bool) list
        | NoMatch

    let internal returnEmptyMatch () = Match List.Empty

    let private (&&~) lhs rhs =
        match (lhs, rhs) with
        | Match(asyncLhs), Match(asyncRhs) -> Match(asyncLhs @ asyncRhs)
        | _ -> NoMatch

    /// Check that an infix equality operation is not actually the assignment of a value to a property in a constructor
    /// or a named parameter in a method call.
    let private notPropertyInitialisationOrNamedParameter arguments leftExpr opExpr =
        match (leftExpr, opExpr) with
        | ExpressionUtilities.Identifier([ident], _), ExpressionUtilities.Identifier([opIdent], _) when opIdent.idText = "op_Equality" ->
            match arguments.FSharpCheckFileResults with
            | Some checkFile ->
                let symbolUse =
                    checkFile.GetSymbolUseAtLocation(
                        ident.idRange.StartLine, ident.idRange.EndColumn, String.Empty, [ident.idText])

                match symbolUse with
                | Some symbolUse ->
                    let checkSymbol () = 
                        match symbolUse.Symbol with
                        | :? FSharpParameter
                        | :? FSharpField -> false
                        | :? FSharpMemberOrFunctionOrValue as element -> not element.IsProperty
                        | _ -> true
                    checkSymbol
                    |> List.singleton
                    |> Match
                | None ->
                    // Symbol resolution failed, fall back to breadcrumb checking
                    match filterParens arguments.Breadcrumbs with
                    | PossiblyInMethod
                    | PossiblyInConstructor -> NoMatch
                    | _ -> Match List.Empty
            | None ->
                /// Check if in `new` expr or function application (either could be a constructor).
                match filterParens arguments.Breadcrumbs with
                | PossiblyInMethod
                | PossiblyInConstructor -> NoMatch
                | _ -> Match List.Empty
        | _ -> Match List.Empty

    [<TailCall>]
    let rec matchHintExpr (continuation: unit -> HintMatch) arguments =
        let expr = removeParens arguments.Expression
        let arguments = { arguments with Expression = expr }

        (continuation ()) &&~
        match arguments.Hint with
        | Expression.Variable(variable) when Map.containsKey variable arguments.LambdaArguments ->
            match expr with
            | AstNode.Expression(ExpressionUtilities.Identifier([identifier], _))
                    when identifier.idText = arguments.LambdaArguments.[variable] ->
                Match(List.Empty)
            | _ -> NoMatch
        | Expression.Variable(var) ->
            match expr with
            | AstNode.Expression(expr) -> arguments.MatchedVariables.TryAdd(var, expr) |> ignore<bool>
            | _ -> ()
            Match(List.Empty)
        | Expression.Wildcard ->
            Match(List.Empty)
        | Expression.Null
        | Expression.Constant(_)
        | Expression.Identifier(_) ->
            if matchExpr expr = Some(arguments.Hint) then Match(List.Empty)
            else NoMatch
        | Expression.Parentheses(hint) ->
            arguments.SubHint(expr, hint) |> matchHintExpr returnEmptyMatch
        | Expression.Tuple(_) ->
            matchTuple arguments
        | Expression.List(_) ->
            matchList arguments
        | Expression.Array(_) ->
            matchArray arguments
        | Expression.If(_) ->
            matchIf arguments
        | Expression.AddressOf(_) ->
            matchAddressOf arguments
        | Expression.PrefixOperator(_) ->
            matchPrefixOperation arguments
        | Expression.InfixOperator(_) ->
            matchInfixOperation arguments
        | Expression.FunctionApplication(_) ->
            matchFunctionApplication arguments
        | Expression.Lambda(_) -> matchLambda arguments
        | Expression.LambdaArg(_)
        | Expression.LambdaBody(_) -> NoMatch
        | Expression.Else(_) -> NoMatch

    and [<TailCall>] private matchFunctionApplication arguments =
        match (arguments.Expression, arguments.Hint) with
        | FuncApp(exprs, _), Expression.FunctionApplication(hintExprs) ->
            let expressions = List.map AstNode.Expression exprs
            doExpressionsMatch expressions hintExprs arguments
        | _ -> NoMatch

    and [<TailCall>] private doExpressionsMatch expressions hintExpressions (arguments:Arguments) =
        if List.length expressions = List.length hintExpressions then
            let subHints = 
                (expressions, hintExpressions)
                ||> List.map2 (fun expr hint -> arguments.SubHint(expr, hint))
            
            let rec innerDoExpressionsMatch args: HintMatch =
                match args with
                | head::tail -> 
                    head |> matchHintExpr (fun () -> innerDoExpressionsMatch tail)
                | [] -> Match(List.Empty)

            innerDoExpressionsMatch subHints
        else
            NoMatch

    and [<TailCall>] private matchIf arguments =
        match (arguments.Expression, arguments.Hint) with
        | (AstNode.Expression(SynExpr.IfThenElse(cond, expr, None, _, _, _, _)),
           Expression.If(hintCond, hintExpr, None)) ->
            matchHintExpr
                (fun () -> (arguments.SubHint(Expression expr, hintExpr) |> matchHintExpr returnEmptyMatch))
                (arguments.SubHint(Expression cond, hintCond))
        | (AstNode.Expression(SynExpr.IfThenElse(cond, expr, Some(elseExpr), _, _, _, _)),
           Expression.If(hintCond, hintExpr, Some(Expression.Else(hintElseExpr)))) ->
            matchHintExpr
                (fun () -> 
                    matchHintExpr
                        (fun () -> arguments.SubHint(Expression expr, hintExpr) |> matchHintExpr returnEmptyMatch)
                        (arguments.SubHint(Expression elseExpr, hintElseExpr)))
                (arguments.SubHint(Expression cond, hintCond))
            
        | _ -> NoMatch

    and [<TailCall>] matchLambda arguments =
        match (arguments.Expression, arguments.Hint) with
        | Lambda({ Arguments = args; Body = body }, _), Expression.Lambda(lambdaArgs, LambdaBody(Expression.LambdaBody(lambdaBody))) ->
            match matchLambdaArguments lambdaArgs args with
            | LambdaMatch.Match(lambdaArguments) ->
                matchHintExpr
                    returnEmptyMatch
                    { arguments.SubHint(AstNode.Expression(body), lambdaBody) with LambdaArguments = lambdaArguments }
            | LambdaMatch.NoMatch -> NoMatch
        | _ -> NoMatch

    and [<TailCall>] private matchTuple arguments =
        match (arguments.Expression, arguments.Hint) with
        | AstNode.Expression(SynExpr.Tuple(_, expressions, _, _)), Expression.Tuple(hintExpressions) ->
            let expressions = List.map AstNode.Expression expressions
            doExpressionsMatch expressions hintExpressions arguments
        | _ -> NoMatch

    and [<TailCall>] private matchList arguments =
        match (arguments.Expression, arguments.Hint) with
        | AstNode.Expression(SynExpr.ArrayOrList(false, expressions, _)), Expression.List(hintExpressions) ->
            let expressions = List.map AstNode.Expression expressions
            doExpressionsMatch expressions hintExpressions arguments
        | AstNode.Expression(SynExpr.ArrayOrListComputed(false, expression, _)), Expression.List([hintExpression]) ->
            arguments.SubHint(AstNode.Expression(expression), hintExpression) |> matchHintExpr returnEmptyMatch
        | _ -> NoMatch

    and [<TailCall>] private matchArray arguments =
        match (arguments.Expression, arguments.Hint) with
        | AstNode.Expression(SynExpr.ArrayOrList(true, expressions, _)), Expression.Array(hintExpressions) ->
            let expressions = List.map AstNode.Expression expressions
            doExpressionsMatch expressions hintExpressions arguments
        | AstNode.Expression(SynExpr.ArrayOrListComputed(true, expression, _)), Expression.Array([hintExpression]) ->
            arguments.SubHint(AstNode.Expression(expression), hintExpression) |> matchHintExpr returnEmptyMatch
        | _ -> NoMatch

    and [<TailCall>] private matchInfixOperation arguments =
        match (arguments.Expression, arguments.Hint) with
        | (AstNode.Expression(SynExpr.App(_, true, (ExpressionUtilities.Identifier(_) as opExpr), SynExpr.Tuple(_, [leftExpr; rightExpr], _, _), _)),
           Expression.InfixOperator(op, left, right)) ->
            matchHintExpr
                (fun () ->
                    matchHintExpr
                        (fun () ->
                            matchHintExpr
                                (fun () -> notPropertyInitialisationOrNamedParameter arguments leftExpr opExpr)
                                (arguments.SubHint(AstNode.Expression(rightExpr), right)))
                        (arguments.SubHint(AstNode.Expression(leftExpr), left)))
                (arguments.SubHint(AstNode.Expression(opExpr), op))
        | (AstNode.Expression(SynExpr.App(_, _, infixExpr, rightExpr, _)),
           Expression.InfixOperator(op, left, right)) ->

            match removeParens <| AstNode.Expression(infixExpr) with
            | AstNode.Expression(SynExpr.App(_, true, opExpr, leftExpr, _)) ->
                matchHintExpr
                    (fun () ->
                        matchHintExpr
                            (fun () ->
                                matchHintExpr
                                    (fun () -> notPropertyInitialisationOrNamedParameter arguments leftExpr opExpr)
                                    (arguments.SubHint(AstNode.Expression(rightExpr), right)))
                            (arguments.SubHint(AstNode.Expression(leftExpr), left)))
                    (arguments.SubHint(AstNode.Expression(opExpr), op))
            | _ -> NoMatch
        | _ -> NoMatch

    and [<TailCall>] private matchPrefixOperation arguments =
        match (arguments.Expression, arguments.Hint) with
        | (AstNode.Expression(SynExpr.App(_, _, opExpr, rightExpr, _)),
           Expression.PrefixOperator(Expression.Identifier([op]), expr)) ->
            matchHintExpr
                (fun () -> arguments.SubHint(AstNode.Expression(rightExpr), expr) |> matchHintExpr returnEmptyMatch)
                (arguments.SubHint(AstNode.Expression(opExpr), Expression.Identifier([op])))
        | _ -> NoMatch

    and [<TailCall>] private matchAddressOf arguments =
        match (arguments.Expression, arguments.Hint) with
        | AstNode.Expression(SynExpr.AddressOf(synSingleAmp, addrExpr, _, _)), Expression.AddressOf(singleAmp, expr) when synSingleAmp = singleAmp ->
            arguments.SubHint(AstNode.Expression(addrExpr), expr) |> matchHintExpr returnEmptyMatch
        | _ -> NoMatch

module private MatchPattern =

    let private matchPattern = function
        | SynPat.LongIdent(ident, _, _, _, _, _) ->
            let identifier = List.map (fun (ident: Ident) -> ident.idText) (ident.LongIdent)
            Some(Pattern.Identifier(identifier))
        | SynPat.Const(constant, _) ->
            matchConst constant |> Option.map Pattern.Constant
        | SynPat.Null(_) ->
            Some(Pattern.Null)
        | _ -> None

    /// Extracts a pattern from parentheses e.g. ((x)) -> x
    let rec private removeParens = function
        | SynPat.Paren(pattern, _) -> removeParens pattern
        | pat -> pat

    let internal returnTrue () = true

    [<TailCall>]
    let rec matchHintPattern (continuation: unit -> bool) (pattern, hint) =
        let pattern = removeParens pattern

        (continuation ()) &&
        match hint with
        | Pattern.Variable(_)
        | Pattern.Wildcard ->
            true
        | Pattern.Null
        | Pattern.Constant(_)
        | Pattern.Identifier(_) ->
            matchPattern pattern = Some(hint)
        | Pattern.Cons(_) ->
            matchConsPattern (pattern, hint)
        | Pattern.Or(_) ->
            matchOrPattern (pattern, hint)
        | Pattern.Parentheses(hint) ->
            matchHintPattern returnTrue (pattern, hint)
        | Pattern.Tuple(_) ->
            matchTuple (pattern, hint)
        | Pattern.List(_) ->
            matchList (pattern, hint)
        | Pattern.Array(_) ->
            matchArray (pattern, hint)

    and [<TailCall>] private doPatternsMatch patterns hintExpressions =
        if List.length patterns = List.length hintExpressions then
            let rec innerDoPatternsMatch lst =
                match lst with
                | (pattern, hintExpression) :: tail ->
                    matchHintPattern
                        (fun () -> innerDoPatternsMatch tail)
                        (pattern, hintExpression)
                | [] -> true

            innerDoPatternsMatch (List.zip patterns hintExpressions)
        else
            false

    and [<TailCall>] private matchList (pattern, hint) =
        match (pattern, hint) with
        | SynPat.ArrayOrList(false, patterns, _), Pattern.List(hintExpressions) ->
            doPatternsMatch patterns hintExpressions
        | _ -> false

    and [<TailCall>] private matchArray (pattern, hint) =
        match (pattern, hint) with
        | SynPat.ArrayOrList(true, patterns, _), Pattern.Array(hintExpressions) ->
            doPatternsMatch patterns hintExpressions
        | _ -> false

    and [<TailCall>] private matchTuple (pattern, hint) =
        match (pattern, hint) with
        | SynPat.Tuple(_, patterns, _, _), Pattern.Tuple(hintExpressions) ->
            doPatternsMatch patterns hintExpressions
        | _ -> false

    and [<TailCall>] private matchConsPattern (pattern, hint) =
        match (pattern, hint) with
        | Cons(leftPattern, rightPattern), Pattern.Cons(left, right) ->
            matchHintPattern 
                (fun () -> matchHintPattern returnTrue (rightPattern, right))
                (leftPattern, left)
        | _ -> false

    and [<TailCall>] private matchOrPattern (pattern, hint) =
        match (pattern, hint) with
        | SynPat.Or(leftPattern, rightPattern, _, _), Pattern.Or(left, right) ->
            matchHintPattern
                (fun () -> matchHintPattern returnTrue (rightPattern, right))
                (leftPattern, left)
        | _ -> false

module private FormatHint =
    let private constantToString = function
        | Constant.Bool(value) -> if value then "true" else "false"
        | Constant.Int16(value) -> $"{value}s"
        | Constant.Int32(value) -> $"{value}"
        | Constant.Int64(value) -> $"{value}L"
        | Constant.UInt16(value) -> $"{value}us"
        | Constant.UInt32(value) -> $"{value}u"
        | Constant.UInt64(value) -> $"{value}UL"
        | Constant.Byte(value) -> $"{value}uy"
        | Constant.Bytes(value) -> $"{value}"
        | Constant.Char(value) -> $"'{value}'"
        | Constant.Decimal(value) -> $"{value}m"
        | Constant.Double(value) -> $"{value}"
        | Constant.SByte(value) -> $"{value}y"
        | Constant.Single(value) -> $"{value}f"
        | Constant.String(value) -> $"\"{value}\""
        | Constant.UIntPtr(value) -> $"{value}"
        | Constant.IntPtr(value) -> $"{value}"
        | Constant.UserNum(value, _) -> $"{value}"
        | Constant.Unit -> "()"

    let private surroundExpressionsString hintToString left right sep expressions =
        let inside =
            expressions
            |> List.map hintToString
            |> String.concat sep

        left + inside + right

    let private opToString = function
        | Expression.Identifier(identifier) -> String.concat "." identifier
        | expression ->
            Debug.Assert(false, $"Expected operator to be an expression identifier, but was {expression.ToString()}")
            String.Empty

    // very hard to turn into tail-recursive form because of the way it operates
    // (convert sub-expressions to strings and then combine them)
    // fsharplint:disable EnsureTailCallDiagnosticsInRecursiveFunctions
    let rec toString (config: ToStringConfig) =
        let toString hintNode =
            toString
                {
                    Replace = config.Replace
                    ParentAstNode = config.ParentAstNode
                    Args = config.Args
                    MatchedVariables = config.MatchedVariables
                    ParentHintNode = (Some config.HintNode)
                    HintNode = hintNode
                }

        let str =
            match config.HintNode with
            | HintExpr(Expression.Variable(varChar)) when config.Replace ->
                match config.MatchedVariables.TryGetValue varChar with
                | true, expr ->
                    match ExpressionUtilities.tryFindTextOfRange expr.Range config.Args.FileContent with
                    | Some(replacement) -> replacement
                    | _ -> varChar.ToString()
                | _ -> varChar.ToString()
            | HintExpr(Expression.Variable(x))
            | HintPat(Pattern.Variable(x)) -> x.ToString()
            | HintExpr(Expression.Wildcard)
            | HintPat(Pattern.Wildcard) -> "_"
            | HintExpr(Expression.Constant(constant))
            | HintPat(Pattern.Constant(constant)) ->
                constantToString constant
            | HintExpr(Expression.Identifier(identifier))
            | HintPat(Pattern.Identifier(identifier)) ->
                identifier
                |> List.map (fun each ->
                    if PrettyNaming.IsOperatorDisplayName each then
                        $"( %s{each} )"
                    else
                        each)
                |> String.concat "."
            | HintExpr(Expression.FunctionApplication(expressions)) ->
                surroundExpressionsString (HintExpr >> toString) String.Empty String.Empty " " expressions
            | HintExpr(Expression.InfixOperator(operator, leftHint, rightHint)) ->
                $"{toString (HintExpr leftHint)} {opToString operator} {toString (HintExpr rightHint)}"
            | HintPat(Pattern.Cons(leftHint, rightHint)) ->
                $"{toString (HintPat leftHint)}::{toString (HintPat rightHint)}"
            | HintPat(Pattern.Or(leftHint, rightHint)) ->
                $"{toString (HintPat leftHint)} | {toString (HintPat rightHint)}"
            | HintExpr(Expression.AddressOf(singleAmp, hint)) ->
                (if singleAmp then "&" else "&&") + toString (HintExpr hint)
            | HintExpr(Expression.PrefixOperator(operator, hint)) ->
                $"{opToString operator}{toString (HintExpr hint)}"
            | HintExpr(Expression.Parentheses(hint)) -> $"({toString (HintExpr hint)})"
            | HintPat(Pattern.Parentheses(hint)) -> $"({toString (HintPat hint)})"
            | HintExpr(Expression.Lambda(arguments, LambdaBody(body))) ->
                $"fun {lambdaArgumentsToString config.Replace config.ParentAstNode config.Args config.MatchedVariables arguments} -> {toString (HintExpr body)}"
            | HintExpr(Expression.LambdaArg(argument)) ->
                toString (HintExpr argument)
            | HintExpr(Expression.LambdaBody(body)) ->
                toString (HintExpr body)
            | HintExpr(Expression.Tuple(expressions)) ->
                surroundExpressionsString (HintExpr >> toString) "(" ")" "," expressions
            | HintExpr(Expression.List(expressions)) ->
                surroundExpressionsString (HintExpr >> toString) "[" "]" ";" expressions
            | HintExpr(Expression.Array(expressions)) ->
                surroundExpressionsString (HintExpr >> toString) "[|" "|]" ";" expressions
            | HintPat(Pattern.Tuple(expressions)) ->
                surroundExpressionsString (HintPat >> toString) "(" ")" "," expressions
            | HintPat(Pattern.List(expressions)) ->
                surroundExpressionsString (HintPat >> toString) "[" "]" ";" expressions
            | HintPat(Pattern.Array(expressions)) ->
                surroundExpressionsString (HintPat >> toString) "[|" "|]" ";" expressions
            | HintExpr(Expression.If(cond, expr, None)) ->
                $"if {toString (HintExpr cond)} then {toString (HintExpr expr)}"
            | HintExpr(Expression.If(cond, expr, Some(elseExpr))) ->
                $"if {toString (HintExpr cond)} then {toString (HintExpr expr)} {toString (HintExpr elseExpr)}"
            | HintExpr(Expression.Else(expr)) ->
                $"else {toString (HintExpr expr)}"
            | HintExpr(Expression.Null)
            | HintPat(Pattern.Null) -> "null"
        if config.Replace && Precedence.requiresParenthesis config.MatchedVariables config.HintNode config.ParentAstNode config.ParentHintNode then $"({str})"
        else str
    and private lambdaArgumentsToString replace parentAstNode args matchedVariables (arguments:LambdaArg list) =
        let exprToString expr =
            toString
                {
                    Replace = replace
                    ParentAstNode = parentAstNode
                    Args = args
                    MatchedVariables = matchedVariables
                    ParentHintNode = None
                    HintNode = (HintExpr expr)
                }
        arguments
        |> List.map (fun (LambdaArg expr) -> exprToString expr)
        |> String.concat " "
    // fsharplint:enable EnsureTailCallDiagnosticsInRecursiveFunctions

type HintErrorConfig =
    {
        TypeChecks: (unit -> bool) list
        Hint: Hint
        Args: AstNodeRuleParams
        Range: FSharp.Compiler.Text.Range
        MatchedVariables: Dictionary<char, SynExpr>
        ParentAstNode: AstNode option
    }

let private hintError (config: HintErrorConfig) =
    let toStringConfig =
        {
            ToStringConfig.Replace = false
            ToStringConfig.ParentAstNode = None
            ToStringConfig.Args = config.Args
            ToStringConfig.MatchedVariables = config.MatchedVariables
            ToStringConfig.ParentHintNode = None
            ToStringConfig.HintNode = config.Hint.MatchedNode
        }

    let matched = FormatHint.toString toStringConfig

    match config.Hint.Suggestion with
    | Suggestion.Expr(expr) ->
        let suggestion = FormatHint.toString { toStringConfig with HintNode = (HintExpr expr) }
        let errorFormatString = Resources.GetString("RulesHintRefactor")
        let error = System.String.Format(errorFormatString, matched, suggestion)

        let toText =
            FormatHint.toString
                {
                    toStringConfig with
                        Replace = true
                        ParentAstNode = config.ParentAstNode
                        HintNode = (HintExpr expr)
                }

        let suggestedFix = lazy(
            ExpressionUtilities.tryFindTextOfRange config.Range config.Args.FileContent
            |> Option.map (fun fromText -> { FromText = fromText; FromRange = config.Range; ToText = toText }))

        { Range = config.Range; Message = error; SuggestedFix = Some suggestedFix; TypeChecks = config.TypeChecks }
    | Suggestion.Message(message) ->
        let errorFormatString = Resources.GetString("RulesHintSuggestion")
        let error = System.String.Format(errorFormatString, matched, message)
        { Range = config.Range; Message = error; SuggestedFix = None; TypeChecks = config.TypeChecks }

let private getMethodParameters (checkFile:FSharpCheckFileResults) (methodIdent: SynLongIdent) =
    let symbol =
        checkFile.GetSymbolUseAtLocation(
            methodIdent.Range.StartLine,
            methodIdent.Range.EndColumn,
            String.Empty,
            List.map (fun (ident: Ident) -> ident.idText) methodIdent.LongIdent)

    match symbol with
    | Some(symbol) when (symbol.Symbol :? FSharpMemberOrFunctionOrValue) ->
        let symbol = symbol.Symbol :?> FSharpMemberOrFunctionOrValue

        if symbol.IsMember then Seq.tryHead symbol.CurriedParameterGroups
        else None
    | _ -> None

/// Check a lambda function can be replaced with a function,
/// it will not be if the lambda is automatically getting
/// converted to a delegate type e.g. Func<T>.
let private canReplaceLambdaWithFunction checkFile methodIdent index =
    let parameters = getMethodParameters checkFile methodIdent

    match parameters with
    | Some(parameters) when index < Seq.length parameters ->
        let parameter = parameters.[index]
        not (parameter.Type.HasTypeDefinition && parameter.Type.TypeDefinition.IsDelegate)
    | _ -> true

/// Check if lambda can be replaced with an identifier (cannot in the case when is a parameter with the type of a delegate).
let private (|RequiresCheck|CanBeReplaced|CannotBeReplaced|) (breadcrumbs, range) =
    match filterParens breadcrumbs with
    | AstNode.Expression(SynExpr.Tuple(_, exprs, _, _))::AstNode.Expression(SynExpr.App(ExprAtomicFlag.Atomic, _, SynExpr.DotGet(_, _, methodIdent, _), _, _))::_
    | AstNode.Expression(SynExpr.Tuple(_, exprs, _, _))::AstNode.Expression(SynExpr.App(ExprAtomicFlag.Atomic, _, SynExpr.LongIdent(_, methodIdent, _, _), _, _))::_ ->
        match List.tryFindIndex (fun (expr: SynExpr) -> expr.Range = range) exprs with
        | Some(index) -> RequiresCheck(index, methodIdent)
        | None -> CannotBeReplaced
    | AstNode.Expression(SynExpr.App(ExprAtomicFlag.Atomic, _, SynExpr.DotGet(_, _, methodIdent, _), _, _))::_
    | AstNode.Expression(SynExpr.App(ExprAtomicFlag.Atomic, _, SynExpr.LongIdent(_, methodIdent, _, _), _, _))::_ ->
        RequiresCheck(0, methodIdent)
    | _ -> CanBeReplaced

let private (|SuggestingReplacementOfLambda|OtherSuggestion|) = function
    | HintExpr(Expression.Lambda(_)), Suggestion.Expr(Expression.Identifier(_)) -> SuggestingReplacementOfLambda
    | _ -> OtherSuggestion

let [<Literal>] private MaxBreadcrumbs = 6
let private suggestions = ResizeArray()

let private confirmFuzzyMatch (args:AstNodeRuleParams) (hint:HintParserTypes.Hint) =
    let breadcrumbs = args.GetParents MaxBreadcrumbs
    match (args.AstNode, hint.MatchedNode) with
    | AstNode.Expression(SynExpr.Paren(_)), HintExpr(_)
    | AstNode.Pattern(SynPat.Paren(_)), HintPat(_) -> ()
    | AstNode.Pattern(pattern), HintPat(hintPattern) when MatchPattern.matchHintPattern MatchPattern.returnTrue (pattern, hintPattern) ->
        hintError
            {
                TypeChecks = List.Empty
                Hint = hint
                Args = args
                Range = pattern.Range
                MatchedVariables = (Dictionary<_, _>())
                ParentAstNode = None
            }
        |> suggestions.Add
    | AstNode.Expression(expr), HintExpr(hintExpr) ->
        let arguments =
            { MatchExpression.LambdaArguments = Map.ofList []
              MatchExpression.MatchedVariables = Dictionary<_, _>()
              MatchExpression.Expression = args.AstNode
              MatchExpression.Hint = hintExpr
              MatchExpression.FSharpCheckFileResults = args.CheckInfo
              MatchExpression.Breadcrumbs = breadcrumbs }

        match MatchExpression.matchHintExpr MatchExpression.returnEmptyMatch arguments with
        | MatchExpression.Match(typeChecks) ->
            let suggest checks =
                hintError
                    {
                        TypeChecks = checks
                        Hint = hint
                        Args = args
                        Range = expr.Range
                        MatchedVariables = arguments.MatchedVariables
                        ParentAstNode = (List.tryHead breadcrumbs)
                    }
                |> suggestions.Add

            match (hint.MatchedNode, hint.Suggestion) with
            | SuggestingReplacementOfLambda ->
                match (breadcrumbs, expr.Range) with
                | RequiresCheck(index, methodIdent) ->
                    match args.CheckInfo with
                    | Some checkFile ->
                        let typeCheck = fun () -> canReplaceLambdaWithFunction checkFile methodIdent index
                        suggest (typeCheck ::typeChecks)
                    | None -> ()
                | CanBeReplaced -> suggest typeChecks
                | CannotBeReplaced -> ()
            | OtherSuggestion -> suggest typeChecks
        | MatchExpression.NoMatch -> ()
    | _ -> ()

/// Searches the abstract syntax array for possible hint matches using the hint trie.
/// Any possible matches that are found will be given to the callback function `notify`,
/// any matches found are not guaranteed and it's expected that the caller verify the match.
let private runner (config:Config) (args:AstNodeRuleParams) =
    match config.HintTrie.Lookup.TryGetValue args.NodeHashcode with
    | true, trie -> Helper.Hints.checkTrie (args.NodeIndex + 1) trie args.SyntaxArray (Dictionary<_, _>()) (confirmFuzzyMatch args)
    | false, _ -> ()

    let result = suggestions.ToArray()
    suggestions.Clear()
    result

let rule config =
    AstNodeRule
        {
            Name = "Hints"
            Identifier = Identifiers.Hints
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner config
                    Cleanup = ignore
                }
        }
