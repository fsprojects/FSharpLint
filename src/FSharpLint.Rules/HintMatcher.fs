(*
    FSharpLint, a linter for F#.
    Copyright (C) 2014 Matthew Mcveigh

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

namespace FSharpLint.Framework

module HintMatcher =

    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FParsec
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.HintParser
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadVisitors
    open FSharpLint.Framework.ExpressionUtilities

    [<Literal>]
    let AnalyserName = "Hints"

    let isAnalyserEnabled config =
        (isAnalyserEnabled config AnalyserName).IsSome

    let rec extractSimplePatterns = function
        | SynSimplePats.SimplePats(simplePatterns, _) -> 
            simplePatterns
        | SynSimplePats.Typed(simplePatterns, _, _) -> 
            extractSimplePatterns simplePatterns

    let rec extractIdent = function
        | SynSimplePat.Id(ident, _, isCompilerGenerated, _, _, _) -> (ident, isCompilerGenerated)
        | SynSimplePat.Attrib(simplePattern, _, _)
        | SynSimplePat.Typed(simplePattern, _, _) -> extractIdent simplePattern
        
    [<RequireQualifiedAccess>]
    type LambdaArgumentMatch =
        | Variable of char * string
        | Wildcard of bool /// bool = whether or not argument was actually a wildcard (could be false if the hint argument is a wildcard but the actual argument was not).
        | NoMatch

    let matchLambdaArgument argument argumentToMatch = 
        let simplePatterns = extractSimplePatterns argument

        if not(List.isEmpty simplePatterns) then
            let identifier, isCompilerGenerated = List.head simplePatterns |> extractIdent

            let isWildcard = isCompilerGenerated && identifier.idText.StartsWith("_")

            match argumentToMatch with
                | Argument.Variable(variable) when not isWildcard -> 
                    LambdaArgumentMatch.Variable(variable, identifier.idText)
                | Argument.Wildcard -> 
                    LambdaArgumentMatch.Wildcard(isWildcard)
                | _ -> LambdaArgumentMatch.NoMatch
        else
            LambdaArgumentMatch.NoMatch
            
    [<RequireQualifiedAccess>]
    type LambdaMatch =
        | Match of SynExpr * Map<char, string> * int
        | NoMatch

    let matchLambdaArguments arguments lambdaExpr =
        let rec matchLambdaArguments arguments matches numberOfWildcards = function
            | SynExpr.Lambda(_, _, argument, expr, _) -> 
                match arguments with
                    | head :: tail ->
                        let atEndOfArguments = List.isEmpty tail
                    
                        match matchLambdaArgument argument head with
                            | LambdaArgumentMatch.Variable(var, ident) ->
                                let matches = (var, ident)::matches
                                if atEndOfArguments then
                                    LambdaMatch.Match(expr, Map.ofList matches, numberOfWildcards)
                                else
                                    matchLambdaArguments tail matches numberOfWildcards expr
                            | LambdaArgumentMatch.Wildcard(isWildcard) ->
                                let numberOfWildcards = numberOfWildcards + if isWildcard then 1 else 0

                                if atEndOfArguments then
                                    LambdaMatch.Match(expr, Map.ofList matches, numberOfWildcards)
                                else
                                    matchLambdaArguments tail matches numberOfWildcards expr
                            | LambdaArgumentMatch.NoMatch ->
                                LambdaMatch.NoMatch
                    | [] -> 
                        LambdaMatch.NoMatch
            | _ -> 
                LambdaMatch.NoMatch

        matchLambdaArguments arguments [] 0 lambdaExpr

    /// Converts a SynConst (FSharp AST) into a Constant (hint AST).
    let matchConst = function
        | SynConst.Bool(x) -> Some(Constant.Bool(x))
        | SynConst.Int16(x) -> Some(Constant.Int16(x))
        | SynConst.Int32(x) -> Some(Constant.Int32(x))
        | SynConst.Int64(x) -> Some(Constant.Int64(x))
        | SynConst.UInt16(x) -> Some(Constant.UInt16(x))
        | SynConst.UInt32(x) -> Some(Constant.UInt32(x))
        | SynConst.UInt64(x) -> Some(Constant.UInt64(x))
        | SynConst.Byte(x) -> Some(Constant.Byte(x))
        | SynConst.Bytes(x, _) -> Some(Constant.Bytes(x))
        | SynConst.Char(x) -> Some(Constant.Char(x))
        | SynConst.Decimal(x) -> Some(Constant.Decimal(x))
        | SynConst.Double(x) -> Some(Constant.Double(x))
        | SynConst.SByte(x) -> Some(Constant.SByte(x))
        | SynConst.Single(x) -> Some(Constant.Single(x))
        | SynConst.String(x, _) -> Some(Constant.String(x))
        | SynConst.UIntPtr(x) -> Some(Constant.UIntPtr(unativeint x))
        | SynConst.IntPtr(x) -> Some(Constant.IntPtr(nativeint x))
        | SynConst.UserNum(x, endChar) -> 
            Some(Constant.UserNum(System.Numerics.BigInteger.Parse(x), endChar.[0]))
        | SynConst.Unit -> Some(Constant.Unit)
        | SynConst.UInt16s(_)
        | SynConst.Measure(_) -> None

    module MatchExpression =

        type Arguments =
            {
                LambdaArguments: Map<char, string>
                Expression: SynExpr
                Hint: Expression
                FSharpCheckFileResults: FSharpCheckFileResults option
            }

            with 
                member this.SubHint(expr, hint) =
                    { this with Expression = expr; Hint = hint }

        let private matchExpr = function
            | SynExpr.Ident(ident) -> 
                let ident = identAsDecompiledOpName ident
                Some(Expression.Identifier([ident]))
            | SynExpr.LongIdent(_, ident, _, _) ->
                let identifier = ident.Lid |> List.map (fun x -> x.idText)
                Some(Expression.Identifier(identifier))
            | SynExpr.Const(constant, _) -> 
                matchConst constant |> Option.map Expression.Constant
            | _ -> None

        /// A match clause is generated by the compiler for each wildcard argument, 
        /// this function extracts the body expression of the lambda from those statements.
        let private removeAutoGeneratedMatchesFromLambda numberOfWildcards =
            let rec removeMatchClauses wildcard = function
                | x when wildcard = numberOfWildcards -> Some(x)
                | SynExpr.Match(SequencePointInfoForBinding.NoSequencePointAtInvisibleBinding, 
                                _, 
                                [SynMatchClause.Clause(SynPat.Wild(_), _, expr, _, _)], _, _) ->
                    removeMatchClauses (wildcard + 1) expr
                | _ -> None

            removeMatchClauses 0

        /// Check that an infix equality operation is not actually the assignment of a value to a property in a constructor
        /// or a named parameter in a method call.
        let private notPropertyInitialisationOrNamedParameter arguments leftExpr opExpr =
            match (leftExpr, opExpr) with 
                | SynExpr.Ident(ident), SynExpr.Ident(opIdent) when opIdent.idText = "op_Equality" ->
                    // TODO
                    match arguments.FSharpCheckFileResults with
                        | Some(checkFile) ->
                            let symbolUse = 
                                checkFile.GetSymbolUseAtLocation(ident.idRange.StartLine, ident.idRange.EndColumn, "", [ident.idText])
                                    |> Async.RunSynchronously
                                    
                            match symbolUse with
                                | Some(symbolUse) ->
                                    match symbolUse.Symbol with
                                        | :? FSharpParameter -> false
                                        | :? FSharpMemberOrFunctionOrValue as x -> not x.IsProperty
                                        | _ -> true
                                | None -> true
                        | None -> true
                | _ -> true

        let rec matchHintExpr arguments =
            let expr = removeParens arguments.Expression
            let arguments = { arguments with Expression = expr }

            match arguments.Hint with
                | Expression.Variable(variable) when arguments.LambdaArguments |> Map.containsKey variable ->
                    match expr with
                        | SynExpr.Ident(identifier) 
                            when identifier.idText = arguments.LambdaArguments.[variable] -> 
                                true
                        | _ -> false
                | Expression.Variable(_)
                | Expression.Wildcard ->
                    true
                | Expression.Constant(_)
                | Expression.Identifier(_) ->
                    matchExpr expr = Some(arguments.Hint)
                | Expression.FunctionApplication(_) ->
                    matchFunctionApplication arguments
                | Expression.InfixOperator(_) ->
                    matchInfixOperation arguments
                | Expression.PrefixOperator(_) ->
                    matchPrefixOperation arguments
                | Expression.Parentheses(hint) -> 
                    arguments.SubHint(expr, hint) |> matchHintExpr
                | Expression.Lambda(_) -> 
                    matchLambda arguments
                | Expression.Tuple(_) ->
                    matchTuple arguments
                | Expression.List(_) ->
                    matchList arguments
                | Expression.Array(_) ->
                    matchArray arguments
                | Expression.If(_) ->
                    matchIf arguments

        and private doExpressionsMatch expressions hintExpressions (arguments: Arguments) =
            List.length expressions = List.length hintExpressions &&
                (expressions, hintExpressions)
                    ||> List.forall2 (fun x y -> arguments.SubHint(x, y) |> matchHintExpr)

        and private matchIf arguments =
            match (arguments.Expression, arguments.Hint) with
                | SynExpr.IfThenElse(cond, expr, None, _, _, _, _), Expression.If(hintCond, hintExpr, None) -> 
                    arguments.SubHint(cond, hintCond) |> matchHintExpr &&
                    arguments.SubHint(expr, hintExpr) |> matchHintExpr
                | SynExpr.IfThenElse(cond, expr, Some(elseExpr), _, _, _, _), Expression.If(hintCond, hintExpr, Some(hintElseExpr)) -> 
                    arguments.SubHint(cond, hintCond) |> matchHintExpr &&
                    arguments.SubHint(expr, hintExpr) |> matchHintExpr &&
                    arguments.SubHint(elseExpr, hintElseExpr) |> matchHintExpr
                | _ -> false

        and private matchFunctionApplication arguments =
            match (arguments.Expression, arguments.Hint) with
                | SynExpr.App(_) as application, Expression.FunctionApplication(hintExpressions) -> 
                    let expressions = flattenFunctionApplication application

                    doExpressionsMatch expressions hintExpressions arguments
                | _ -> false

        and private matchLambda arguments =
            match (arguments.Expression, arguments.Hint) with
                | SynExpr.Lambda(_) as lambda, Expression.Lambda(lambdaHint) -> 
                    match matchLambdaArguments lambdaHint.Arguments lambda with
                        | LambdaMatch.Match(bodyExpr, lambdaArguments, numberOfWildcards) -> 
                            match removeAutoGeneratedMatchesFromLambda numberOfWildcards bodyExpr with
                                | Some(bodyExpr) -> 
                                    { arguments.SubHint(bodyExpr, lambdaHint.Body) with LambdaArguments = lambdaArguments } |> matchHintExpr
                                | None -> false
                        | LambdaMatch.NoMatch -> false
                | _ -> false

        and private matchTuple arguments =
            match (arguments.Expression, arguments.Hint) with
                | SynExpr.Tuple(expressions, _, _), Expression.Tuple(hintExpressions) ->
                    doExpressionsMatch expressions hintExpressions arguments
                | _ -> false

        and private matchList arguments =
            match (arguments.Expression, arguments.Hint) with
                | SynExpr.ArrayOrList(false, expressions, _), Expression.List(hintExpressions) ->
                    doExpressionsMatch expressions hintExpressions arguments
                | SynExpr.ArrayOrListOfSeqExpr(false, SynExpr.CompExpr(true, _, expression, _), _), Expression.List([hintExpression]) ->
                    arguments.SubHint(expression, hintExpression) |> matchHintExpr
                | _ -> false

        and private matchArray arguments =
            match (arguments.Expression, arguments.Hint) with
                | SynExpr.ArrayOrList(true, expressions, _), Expression.Array(hintExpressions) ->
                    doExpressionsMatch expressions hintExpressions arguments
                | SynExpr.ArrayOrListOfSeqExpr(true, SynExpr.CompExpr(true, _, expression, _), _), Expression.Array([hintExpression]) ->
                    arguments.SubHint(expression, hintExpression) |> matchHintExpr
                | _ -> false

        and private matchInfixOperation arguments =
            match (arguments.Expression, arguments.Hint) with
                | SynExpr.App(_, true, (SynExpr.Ident(_) as opExpr), SynExpr.Tuple([leftExpr; rightExpr], _, _), _), 
                        Expression.InfixOperator(op, left, right) ->
                    arguments.SubHint(opExpr, Expression.Identifier([op])) |> matchHintExpr &&
                    arguments.SubHint(rightExpr, right) |> matchHintExpr &&
                    arguments.SubHint(leftExpr, left) |> matchHintExpr
                | SynExpr.App(_, _, infixExpr, rightExpr, _) as application, 
                        Expression.InfixOperator(op, left, right) -> 

                    match removeParens infixExpr with
                        | SynExpr.App(_, true, opExpr, leftExpr, _) ->
                            arguments.SubHint(opExpr, Expression.Identifier([op])) |> matchHintExpr &&
                            arguments.SubHint(leftExpr, left) |> matchHintExpr &&
                            arguments.SubHint(rightExpr, right) |> matchHintExpr &&
                            notPropertyInitialisationOrNamedParameter arguments leftExpr opExpr
                        | _ -> false
                | _ -> false

        and private matchPrefixOperation arguments =
            match (arguments.Expression, arguments.Hint) with
                | SynExpr.App(_, _, opExpr, rightExpr, _) as application, 
                        Expression.PrefixOperator(op, expr) -> 
                    arguments.SubHint(opExpr, Expression.Identifier(["~" + op])) |> matchHintExpr &&
                    arguments.SubHint(rightExpr, expr) |> matchHintExpr
                | SynExpr.AddressOf(_, addrExpr, _, _), Expression.PrefixOperator(op, expr) 
                        when op = "&" || op = "&&" ->
                    arguments.SubHint(addrExpr, expr) |> matchHintExpr
                | _ -> false

    module MatchPattern =

        let private matchPattern = function
            | SynPat.LongIdent(ident, _, _, _, _, _) ->
                let identifier = ident.Lid |> List.map (fun x -> x.idText)
                Some(Expression.Identifier(identifier))
            | SynPat.Const(constant, _) -> 
                matchConst constant |> Option.map Expression.Constant
            | _ -> None

        /// Extracts a pattern from parentheses e.g. ((x)) -> x
        let rec private removeParens = function
            | SynPat.Paren(x, _) -> removeParens x
            | x -> x
    
        let rec matchHintPattern (pattern, hint) =
            let pattern = removeParens pattern

            match hint with
                | Expression.Variable(_)
                | Expression.Wildcard ->
                    true
                | Expression.Constant(_)
                | Expression.Identifier(_) ->
                    matchPattern pattern = Some(hint)
                | Expression.InfixOperator("::", _, _) ->
                    matchConsPattern (pattern, hint)
                | Expression.InfixOperator("|", _, _) ->
                    matchOrPattern (pattern, hint)
                | Expression.InfixOperator("&", _, _) ->
                    matchAndPattern (pattern, hint)
                | Expression.Parentheses(hint) -> 
                    matchHintPattern (pattern, hint)
                | Expression.Tuple(_) ->
                    matchTuple (pattern, hint)
                | Expression.List(_) ->
                    matchList (pattern, hint)
                | Expression.Array(_) ->
                    matchArray (pattern, hint)
                | Expression.FunctionApplication(_)
                | Expression.Lambda(_)
                | Expression.If(_)
                | Expression.InfixOperator(_)
                | Expression.PrefixOperator(_) ->
                    false

        and private doPatternsMatch patterns hintExpressions =
            List.length patterns = List.length hintExpressions &&
                (patterns, hintExpressions)
                    ||> List.forall2 (fun x y -> matchHintPattern (x, y))

        and private matchList (pattern, hint) =
            match (pattern, hint) with
                | SynPat.ArrayOrList(false, patterns, _), Expression.List(hintExpressions) ->
                    doPatternsMatch patterns hintExpressions
                | _ -> false

        and private matchArray (pattern, hint) =
            match (pattern, hint) with
                | SynPat.ArrayOrList(true, patterns, _), Expression.Array(hintExpressions) ->
                    doPatternsMatch patterns hintExpressions
                | _ -> false

        and private matchTuple (pattern, hint) =
            match (pattern, hint) with
                | SynPat.Tuple(patterns, _), Expression.Tuple(hintExpressions) ->
                    doPatternsMatch patterns hintExpressions
                | _ -> false

        and private matchConsPattern (pattern, hint) =
            match (pattern, hint) with
                | SynPat.LongIdent(
                                    LongIdentWithDots([ident],_), 
                                    _, 
                                    _, 
                                    Pats([SynPat.Tuple([leftPattern;rightPattern], _)]), 
                                    _, 
                                    _), Expression.InfixOperator("::", left, right)
                        when ident.idText = "op_ColonColon" ->
                    matchHintPattern (leftPattern, left) && matchHintPattern (rightPattern, right)
                | _ -> false

        and private matchOrPattern (pattern, hint) =
            match (pattern, hint) with
                | SynPat.Or(leftPattern, rightPattern, _), Expression.InfixOperator("|", left, right) ->
                    matchHintPattern (leftPattern, left) && matchHintPattern (rightPattern, right)
                | _ -> false

        and private matchAndPattern (pattern, hint) =
            let rec matchAndPatterns = function 
                | rightPattern::patterns, Expression.InfixOperator(
                                                                    "&", 
                                                                    (Expression.InfixOperator("&", _, _) as left), 
                                                                    right) -> 
                    matchHintPattern (rightPattern, right) && matchAndPatterns (patterns, left)
                | rightPattern::leftPattern::_ & _::patterns, Expression.InfixOperator("&", left, right) -> 
                    let isMatch = matchHintPattern (leftPattern, left) && matchHintPattern (rightPattern, right)

                    isMatch || matchAndPatterns (patterns, hint)
                | _ -> false

            match pattern with
                | SynPat.Ands(patterns, _) -> 
                    matchAndPatterns (List.rev patterns, hint)
                | _ -> false

    /// Gets a list of hints from the config file.
    let getHints config = 
        let analyser = Map.find AnalyserName config.Analysers

        let parseHint hint =
            match run phint hint with
                | Success(hint, _, _) -> hint
                | Failure(error, _, _) -> 
                    raise <| ConfigurationException("Failed to parse hint: " + hint + "\n" + error)

        match Map.find "Hints" analyser.Settings with
            | Hints(stringHints) -> 
                stringHints 
                    |> List.filter (System.String.IsNullOrEmpty >> not)
                    |> List.map parseHint
            | _ -> 
                raise <| ConfigurationException("Expected the Hints rule to contain a list of hints")

    /// Memoized getHints.
    let getHintsFromConfig = 
        let hints = ref None

        let getMemoizedHints config =
            match !hints with
                | None ->
                    let parsedHints = getHints config
                    hints := Some(parsedHints)
                    parsedHints
                | Some(hints) -> hints

        getMemoizedHints

    let lambdaArgumentsToString (arguments:Argument list) = 
        let argumentToString = function
            | Argument.Wildcard -> "_"
            | Argument.Variable(x) -> x.ToString()

        arguments
            |> List.map argumentToString
            |> String.concat " "

    let constantToString = function
        | Constant.Bool(x) -> if x then "true" else "false"
        | Constant.Int16(x) -> x.ToString() + "s"
        | Constant.Int32(x) -> x.ToString()
        | Constant.Int64(x) -> x.ToString() + "L"
        | Constant.UInt16(x) -> x.ToString() + "us"
        | Constant.UInt32(x) -> x.ToString() + "u"
        | Constant.UInt64(x) -> x.ToString() + "UL"
        | Constant.Byte(x) -> x.ToString() + "uy"
        | Constant.Bytes(x) -> x.ToString()
        | Constant.Char(x) -> "'" + x.ToString() + "'"
        | Constant.Decimal(x) -> x.ToString() + "m"
        | Constant.Double(x) -> x.ToString()
        | Constant.SByte(x) -> x.ToString() + "y"
        | Constant.Single(x) -> x.ToString() + "f"
        | Constant.String(x) -> "\"" + x + "\""
        | Constant.UIntPtr(x) -> x.ToString()
        | Constant.IntPtr(x) -> x.ToString()
        | Constant.UserNum(x, char) -> x.ToString()
        | Constant.Unit -> "()"

    let private surroundExpressionsString hintToString left right sep expressions =
        let inside =
            expressions 
                |> List.map hintToString
                |> String.concat sep

        left + inside + right

    let rec hintToString = function
        | Expression.Variable(x) -> x.ToString()
        | Expression.Wildcard -> "_"
        | Expression.Constant(constant) -> 
            constantToString constant
        | Expression.Identifier(identifier) ->
            String.concat "." identifier
        | Expression.FunctionApplication(expressions) ->
            expressions |> surroundExpressionsString hintToString "" "" " "
        | Expression.InfixOperator(operator, leftHint, rightHint) ->
            hintToString leftHint + operator + hintToString rightHint
        | Expression.PrefixOperator(operator, hint) ->
            operator + hintToString hint
        | Expression.Parentheses(hint) -> 
            "(" + hintToString hint + ")"
        | Expression.Lambda(lambda) -> 
            "fun " + lambdaArgumentsToString lambda.Arguments + " -> " + hintToString lambda.Body
        | Expression.Tuple(expressions) ->
            expressions |> surroundExpressionsString hintToString "(" ")" ","
        | Expression.List(expressions) ->
            expressions |> surroundExpressionsString hintToString "[" "]" ";"
        | Expression.Array(expressions) ->
            expressions |> surroundExpressionsString hintToString "[|" "|]" ";"
        | Expression.If(cond, expr, None) ->
            "if " + hintToString cond + " then " + hintToString expr
        | Expression.If(cond, expr, Some(elseExpr)) ->
            "if " + hintToString cond + " then " + hintToString expr +
            " else " + hintToString elseExpr

    let hintError hint visitorInfo range =
        let matched = hintToString hint.Match
        let suggestion = hintToString hint.Suggestion

        let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesHintRefactor")
        let error = System.String.Format(errorFormatString, matched, suggestion)

        visitorInfo.PostError range error

    let visitor getHints visitorInfo checkFile (astNode:CurrentNode) = 
        if isAnalyserEnabled visitorInfo.Config && astNode.IsSuppressed(AnalyserName) |> not then
            match astNode.Node with
                | AstNode.Expression(SynExpr.Paren(_)) -> Continue
                | AstNode.Expression(expr) -> 
                    for hint in getHints visitorInfo.Config do
                        let arguments =
                            {
                                MatchExpression.LambdaArguments = Map.ofList []
                                MatchExpression.Expression = expr
                                MatchExpression.Hint = hint.Match
                                MatchExpression.FSharpCheckFileResults = checkFile
                            }

                        if MatchExpression.matchHintExpr arguments then
                            hintError hint visitorInfo expr.Range

                    Continue
                | AstNode.Pattern(SynPat.Paren(_)) -> Continue
                | AstNode.Pattern(pattern) ->
                    for hint in getHints visitorInfo.Config do
                        if MatchPattern.matchHintPattern (pattern, hint.Match) then
                            hintError hint visitorInfo pattern.Range
                    
                    Continue
                | _ -> Continue
        else
            Stop

    type RegisterHintVisitor() = 
        let plugin =
            {
                Name = AnalyserName
                Visitor = Ast(visitor getHintsFromConfig)
            }

        interface IRegisterPluginWithConfigChecker with
            member this.RegisterPlugin with get() = plugin

            member this.CheckConfig config = 
                try
                    getHintsFromConfig config |> ignore
                    CheckConfigResult.Success
                with
                    | ConfigurationException(message) ->
                        CheckConfigResult.Failed(message)