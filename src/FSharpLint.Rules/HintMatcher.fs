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

namespace FSharpLint.Rules

module HintMatcher =

    open System.Diagnostics
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.PrettyNaming
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.HintParser
    open FSharpLint.Framework.Configuration
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
        | Variable of variable:char * identifier:string
        | Wildcard
        | NoMatch

    let matchLambdaArgument (LambdaArg.LambdaArg(hintArg), actualArg) = 
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
    type LambdaMatch =
        | Match of Map<char, string>
        | NoMatch

    let matchLambdaArguments (hintArgs:HintParser.LambdaArg list) (actualArgs:SynSimplePats list) =
        if List.length hintArgs <> List.length actualArgs then
            LambdaMatch.NoMatch
        else
            let matches =
                List.zip hintArgs actualArgs
                |> List.map matchLambdaArgument

            let allArgsMatch = matches |> List.forall (function LambdaArgumentMatch.NoMatch -> false | _ -> true)

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

        /// Extracts an expression from parentheses e.g. ((x + 4)) -> x + 4
        let rec removeParens = function
            | AstNode.Expression(SynExpr.Paren(x, _, _, _)) -> x |> AstNode.Expression |> removeParens
            | x -> x

        type Arguments =
            { LambdaArguments: Map<char, string>
              Expression: AstNode
              Hint: Expression
              FSharpCheckFileResults: FSharpCheckFileResults option
              Breadcrumbs: AstNode list }

            with 
                member this.SubHint(expr, hint) =
                    { this with Expression = expr; Hint = hint }

        let private matchExpr = function
            | AstNode.Expression(SynExpr.Ident(ident)) -> 
                let ident = identAsDecompiledOpName ident
                Some(Expression.Identifier([ident]))
            | AstNode.Expression(SynExpr.LongIdent(_, ident, _, _)) ->
                let identifier = ident.Lid |> List.map (fun x -> x.idText)
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
            | AstNode.Expression(SynExpr.Tuple(_))::AstNode.Expression(SynExpr.Paren(_))::AstNode.TypeParameter(_)::AstNode.Expression(SynExpr.New(_))::_
            | AstNode.Expression(SynExpr.Tuple(_))::AstNode.Expression(SynExpr.Paren(_))::AstNode.Expression(SynExpr.New(_))::_
            | AstNode.Expression(SynExpr.Paren(_))::AstNode.TypeParameter(_)::AstNode.Expression(SynExpr.New(_))::_
            | AstNode.Expression(SynExpr.Paren(_))::AstNode.Expression(SynExpr.New(_))::_ ->
                PossiblyInConstructor
            | AstNode.Expression(PossiblyMethodCallOrConstructor)::_
            | AstNode.Expression(SynExpr.Tuple(_))::AstNode.Expression(PossiblyMethodCallOrConstructor)::_
            | AstNode.Expression(SynExpr.Tuple(_))::AstNode.TypeParameter(_)::AstNode.Expression(PossiblyMethodCallOrConstructor)::_ -> 
                PossiblyInMethod
            | _ -> NotInMethod
            
        /// Check that an infix equality operation is not actually the assignment of a value to a property in a constructor
        /// or a named parameter in a method call.
        let private notPropertyInitialisationOrNamedParameter arguments leftExpr opExpr =
            match (leftExpr, opExpr) with 
            | SynExpr.Ident(ident), SynExpr.Ident(opIdent) when opIdent.idText = "op_Equality" ->
                match arguments.FSharpCheckFileResults with
                | Some(checkFile) ->
                    let symbolUse = 
                        checkFile.GetSymbolUseAtLocation(ident.idRange.StartLine, 
                                                         ident.idRange.EndColumn, 
                                                         "", 
                                                         [ident.idText])
                        |> Async.RunSynchronously
                                    
                    match symbolUse with
                    | Some(symbolUse) ->
                        match symbolUse.Symbol with
                        | :? FSharpParameter -> false
                        | :? FSharpMemberOrFunctionOrValue as x -> not x.IsProperty
                        | _ -> true
                    | None -> true
                | None -> 
                    /// Check if in `new` expr or function application (either could be a constructor).
                    match arguments.Breadcrumbs with
                    | PossiblyInMethod 
                    | PossiblyInConstructor -> false
                    | _ -> true
            | _ -> true

        let rec matchHintExpr arguments =
            let expr = removeParens arguments.Expression
            let arguments = { arguments with Expression = expr }

            match arguments.Hint with
            | Expression.Variable(variable) when arguments.LambdaArguments |> Map.containsKey variable ->
                match expr with
                | AstNode.Expression(SynExpr.Ident(identifier)) when identifier.idText = arguments.LambdaArguments.[variable] -> 
                    true
                | _ -> false
            | Expression.Variable(_)
            | Expression.Wildcard ->
                true
            | Expression.Null
            | Expression.Constant(_)
            | Expression.Identifier(_) ->
                matchExpr expr = Some(arguments.Hint)
            | Expression.Parentheses(hint) -> 
                arguments.SubHint(expr, hint) |> matchHintExpr
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
            | Expression.LambdaBody(_) -> false
            | Expression.Else(_) -> false

        and matchFunctionApplication arguments =
            match (arguments.Expression, arguments.Hint) with
            | FuncApp(exprs, _), Expression.FunctionApplication(hintExprs) ->
                let expressions = exprs |> List.map AstNode.Expression
                doExpressionsMatch expressions hintExprs arguments
            | _ -> false

        and doExpressionsMatch expressions hintExpressions (arguments: Arguments) =
            List.length expressions = List.length hintExpressions &&
                (expressions, hintExpressions) ||> List.forall2 (fun x y -> arguments.SubHint(x, y) |> matchHintExpr)

        and private matchIf arguments =
            match (arguments.Expression, arguments.Hint) with
            | AstNode.Expression(SynExpr.IfThenElse(cond, expr, None, _, _, _, _)), 
              Expression.If(hintCond, hintExpr, None) -> 
                arguments.SubHint(Expression cond, hintCond) |> matchHintExpr &&
                arguments.SubHint(Expression expr, hintExpr) |> matchHintExpr
            | AstNode.Expression(SynExpr.IfThenElse(cond, expr, Some(elseExpr), _, _, _, _)), 
              Expression.If(hintCond, hintExpr, Some(Expression.Else(hintElseExpr))) -> 
                arguments.SubHint(Expression cond, hintCond) |> matchHintExpr &&
                arguments.SubHint(Expression expr, hintExpr) |> matchHintExpr &&
                arguments.SubHint(Expression elseExpr, hintElseExpr) |> matchHintExpr
            | _ -> false

        and matchLambda arguments =
            match (arguments.Expression, arguments.Hint) with
            | Lambda({ Arguments = args; Body = body }, _), Expression.Lambda(lambdaArgs, LambdaBody(Expression.LambdaBody(lambdaBody))) -> 
                match matchLambdaArguments lambdaArgs args with
                | LambdaMatch.Match(lambdaArguments) -> 
                    matchHintExpr { arguments.SubHint(AstNode.Expression(body), lambdaBody) with LambdaArguments = lambdaArguments }
                | LambdaMatch.NoMatch -> false
            | _ -> false

        and private matchTuple arguments =
            match (arguments.Expression, arguments.Hint) with
            | AstNode.Expression(SynExpr.Tuple(expressions, _, _)), Expression.Tuple(hintExpressions) ->
                let expressions = List.map AstNode.Expression expressions
                doExpressionsMatch expressions hintExpressions arguments
            | _ -> false

        and private matchList arguments =
            match (arguments.Expression, arguments.Hint) with
            | AstNode.Expression(SynExpr.ArrayOrList(false, expressions, _)), Expression.List(hintExpressions) ->
                let expressions = List.map AstNode.Expression expressions
                doExpressionsMatch expressions hintExpressions arguments
            | AstNode.Expression(SynExpr.ArrayOrListOfSeqExpr(false, SynExpr.CompExpr(true, _, expression, _), _)), Expression.List([hintExpression]) ->
                arguments.SubHint(AstNode.Expression(expression), hintExpression) |> matchHintExpr
            | _ -> false

        and private matchArray arguments =
            match (arguments.Expression, arguments.Hint) with
            | AstNode.Expression(SynExpr.ArrayOrList(true, expressions, _)), Expression.Array(hintExpressions) ->
                let expressions = List.map AstNode.Expression expressions
                doExpressionsMatch expressions hintExpressions arguments
            | AstNode.Expression(SynExpr.ArrayOrListOfSeqExpr(true, SynExpr.CompExpr(true, _, expression, _), _)), Expression.Array([hintExpression]) ->
                arguments.SubHint(AstNode.Expression(expression), hintExpression) |> matchHintExpr
            | _ -> false

        and private matchInfixOperation arguments =
            match (arguments.Expression, arguments.Hint) with
            | AstNode.Expression(SynExpr.App(_, true, (SynExpr.Ident(_) as opExpr), SynExpr.Tuple([leftExpr; rightExpr], _, _), _)), 
                    Expression.InfixOperator(op, left, right) ->
                arguments.SubHint(AstNode.Expression(opExpr), op) |> matchHintExpr &&
                arguments.SubHint(AstNode.Expression(rightExpr), right) |> matchHintExpr &&
                arguments.SubHint(AstNode.Expression(leftExpr), left) |> matchHintExpr
            | AstNode.Expression(SynExpr.App(_, _, infixExpr, rightExpr, _)), 
                    Expression.InfixOperator(op, left, right) -> 

                match removeParens <| AstNode.Expression(infixExpr) with
                | AstNode.Expression(SynExpr.App(_, true, opExpr, leftExpr, _)) ->
                    arguments.SubHint(AstNode.Expression(opExpr), op) |> matchHintExpr &&
                    arguments.SubHint(AstNode.Expression(leftExpr), left) |> matchHintExpr &&
                    arguments.SubHint(AstNode.Expression(rightExpr), right) |> matchHintExpr &&
                    notPropertyInitialisationOrNamedParameter arguments leftExpr opExpr
                | _ -> false
            | _ -> false

        and private matchPrefixOperation arguments =
            match (arguments.Expression, arguments.Hint) with
            | AstNode.Expression(SynExpr.App(_, _, opExpr, rightExpr, _)), 
                    Expression.PrefixOperator(Expression.Identifier([op]), expr) -> 
                arguments.SubHint(AstNode.Expression(opExpr), Expression.Identifier([op])) |> matchHintExpr &&
                arguments.SubHint(AstNode.Expression(rightExpr), expr) |> matchHintExpr
            | _ -> false

        and private matchAddressOf arguments =
            match (arguments.Expression, arguments.Hint) with
            | AstNode.Expression(SynExpr.AddressOf(synSingleAmp, addrExpr, _, _)), Expression.AddressOf(singleAmp, expr) when synSingleAmp = singleAmp ->
                arguments.SubHint(AstNode.Expression(addrExpr), expr) |> matchHintExpr
            | _ -> false

    module MatchPattern =

        let private matchPattern = function
            | SynPat.LongIdent(ident, _, _, _, _, _) ->
                let identifier = ident.Lid |> List.map (fun x -> x.idText)
                Some(Pattern.Identifier(identifier))
            | SynPat.Const(constant, _) -> 
                matchConst constant |> Option.map Pattern.Constant
            | SynPat.Null(_) ->
                Some(Pattern.Null)
            | _ -> None

        /// Extracts a pattern from parentheses e.g. ((x)) -> x
        let rec private removeParens = function
            | SynPat.Paren(x, _) -> removeParens x
            | x -> x
    
        let rec matchHintPattern (pattern, hint) =
            let pattern = removeParens pattern

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
                matchHintPattern (pattern, hint)
            | Pattern.Tuple(_) ->
                matchTuple (pattern, hint)
            | Pattern.List(_) ->
                matchList (pattern, hint)
            | Pattern.Array(_) ->
                matchArray (pattern, hint)

        and private doPatternsMatch patterns hintExpressions =
            List.length patterns = List.length hintExpressions &&
                (patterns, hintExpressions) ||> List.forall2 (fun x y -> matchHintPattern (x, y))

        and private matchList (pattern, hint) =
            match (pattern, hint) with
            | SynPat.ArrayOrList(false, patterns, _), Pattern.List(hintExpressions) ->
                doPatternsMatch patterns hintExpressions
            | _ -> false

        and private matchArray (pattern, hint) =
            match (pattern, hint) with
            | SynPat.ArrayOrList(true, patterns, _), Pattern.Array(hintExpressions) ->
                doPatternsMatch patterns hintExpressions
            | _ -> false

        and private matchTuple (pattern, hint) =
            match (pattern, hint) with
            | SynPat.Tuple(patterns, _), Pattern.Tuple(hintExpressions) ->
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
                                _), Pattern.Cons(left, right)
                    when ident.idText = "op_ColonColon" ->
                matchHintPattern (leftPattern, left) && matchHintPattern (rightPattern, right)
            | _ -> false

        and private matchOrPattern (pattern, hint) =
            match (pattern, hint) with
            | SynPat.Or(leftPattern, rightPattern, _), Pattern.Or(left, right) ->
                matchHintPattern (leftPattern, left) && matchHintPattern (rightPattern, right)
            | _ -> false

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
        | Constant.UserNum(x, _) -> x.ToString()
        | Constant.Unit -> "()"

    let private surroundExpressionsString hintToString left right sep expressions =
        let inside =
            expressions 
            |> List.map hintToString
            |> String.concat sep

        left + inside + right

    let rec hintToString = function
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
            |> List.map DemangleOperatorName
            |> String.concat "."
        | HintExpr(Expression.FunctionApplication(expressions)) ->
            expressions |> surroundExpressionsString (HintExpr >> hintToString) "" "" " "
        | HintExpr(Expression.InfixOperator(Expression.Identifier(opIdent), leftHint, rightHint)) ->
            hintToString (HintExpr leftHint) + (String.concat "." opIdent) + hintToString (HintExpr rightHint)
        | HintPat(Pattern.Cons(leftHint, rightHint)) ->
            hintToString (HintPat leftHint) + "::" + hintToString (HintPat rightHint)
        | HintPat(Pattern.Or(leftHint, rightHint)) ->
            hintToString (HintPat leftHint) + "|" + hintToString (HintPat rightHint)
        | HintExpr(Expression.AddressOf(singleAmp, hint)) ->
            (if singleAmp then "&" else "&&") + hintToString (HintExpr hint)
        | HintExpr(Expression.PrefixOperator(Expression.Identifier([operator]), hint)) ->
            operator + hintToString (HintExpr hint)
        | HintExpr(Expression.Parentheses(hint)) -> "(" + hintToString (HintExpr hint) + ")"
        | HintPat(Pattern.Parentheses(hint)) -> "(" + hintToString (HintPat hint) + ")"
        | HintExpr(Expression.Lambda(arguments, LambdaBody(body))) -> 
            "fun " + lambdaArgumentsToString arguments + " -> " + hintToString (HintExpr body)
        | HintExpr(Expression.LambdaArg(argument)) ->
            hintToString (HintExpr argument)
        | HintExpr(Expression.LambdaBody(body)) ->
            hintToString (HintExpr body)
        | HintExpr(Expression.Tuple(expressions)) ->
            expressions |> surroundExpressionsString (HintExpr >> hintToString) "(" ")" ","
        | HintExpr(Expression.List(expressions)) ->
            expressions |> surroundExpressionsString (HintExpr >> hintToString) "[" "]" ";"
        | HintExpr(Expression.Array(expressions)) ->
            expressions |> surroundExpressionsString (HintExpr >> hintToString) "[|" "|]" ";"
        | HintPat(Pattern.Tuple(expressions)) ->
            expressions |> surroundExpressionsString (HintPat >> hintToString) "(" ")" ","
        | HintPat(Pattern.List(expressions)) ->
            expressions |> surroundExpressionsString (HintPat >> hintToString) "[" "]" ";"
        | HintPat(Pattern.Array(expressions)) ->
            expressions |> surroundExpressionsString (HintPat >> hintToString) "[|" "|]" ";"
        | HintExpr(Expression.If(cond, expr, None)) ->
            "if " + hintToString (HintExpr cond) + " then " + hintToString (HintExpr expr)
        | HintExpr(Expression.If(cond, expr, Some(elseExpr))) ->
            "if " + hintToString (HintExpr cond) + " then " + hintToString (HintExpr expr) + " " + hintToString (HintExpr elseExpr)
        | HintExpr(Expression.Else(expr)) ->
            "else " + hintToString (HintExpr expr)
        | HintExpr(Expression.Null)
        | HintPat(Pattern.Null) -> "null"
    and lambdaArgumentsToString (arguments:LambdaArg list) = 
        arguments
        |> List.map (function LambdaArg(expr) -> hintToString (HintExpr expr))
        |> String.concat " "

    let hintError hint visitorInfo range =
        let matched = hintToString hint.Match

        let error =
            match hint.Suggestion with
            | Suggestion.Expr(expr) -> 
                let suggestion = hintToString (HintExpr expr)
                let errorFormatString = Resources.GetString("RulesHintRefactor")
                System.String.Format(errorFormatString, matched, suggestion)
            | Suggestion.Message(message) -> 
                let errorFormatString = Resources.GetString("RulesHintSuggestion")
                System.String.Format(errorFormatString, matched, message)

        visitorInfo.PostError range error

    let getMethodParameters (checkFile:FSharpCheckFileResults) (methodIdent:LongIdentWithDots) =
        let symbol =
            checkFile.GetSymbolUseAtLocation(
                methodIdent.Range.StartLine,
                methodIdent.Range.EndColumn,
                "", 
                methodIdent.Lid |> List.map (fun x -> x.idText))
                |> Async.RunSynchronously

        match symbol with
        | Some(symbol) when (symbol.Symbol :? FSharpMemberOrFunctionOrValue) -> 
            let symbol = symbol.Symbol :?> FSharpMemberOrFunctionOrValue

            if symbol.IsMember && (not << Seq.isEmpty) symbol.CurriedParameterGroups then
                symbol.CurriedParameterGroups.[0] |> Some
            else
                None
        | _ -> None

    /// Check a lambda function can be replaced with a function,
    /// it will not be if the lambda is automatically getting
    /// converted to a delegate type e.g. Func<T>.
    let lambdaCanBeReplacedWithFunction checkFile breadcrumbs range =
        let isParameterDelegateType index methodIdent =
            match checkFile with
            | Some(checkFile) ->
                let parameters = getMethodParameters checkFile methodIdent

                match parameters with
                | Some(parameters) when index < Seq.length parameters ->
                    let parameter = parameters.[index]

                    parameter.Type.HasTypeDefinition &&
                    parameter.Type.TypeDefinition.IsDelegate
                | _ -> false
            | None ->
                /// When we're unable to check the parameters 
                /// fallback to say it is delegate type.
                true

        match breadcrumbs with
        | AstNode.Expression(SynExpr.Tuple(exprs, _, _))::AstNode.Expression(SynExpr.App(ExprAtomicFlag.Atomic, _, SynExpr.DotGet(_, _, methodIdent, _), _, _))::_ 
        | AstNode.Expression(SynExpr.Tuple(exprs, _, _))::AstNode.Expression(SynExpr.App(ExprAtomicFlag.Atomic, _, SynExpr.LongIdent(_, methodIdent, _, _), _, _))::_ -> 
            let index = exprs |> List.tryFindIndex (fun x -> x.Range = range)

            match index with
            | Some(index) -> not <| isParameterDelegateType index methodIdent
            | None -> false
        | AstNode.Expression(SynExpr.App(ExprAtomicFlag.Atomic, _, SynExpr.DotGet(_, _, methodIdent, _), arg, _))::_
        | AstNode.Expression(SynExpr.App(ExprAtomicFlag.Atomic, _, SynExpr.LongIdent(_, methodIdent, _, _), arg, _))::_ -> 
            not <| isParameterDelegateType 0 methodIdent
        | _ -> true

    let confirmFuzzyMatch visitorInfo checkFile (node:AbstractSyntaxArray.Node) breadcrumbs (hint:HintParser.Hint) =
        match node.Actual, hint.Match with
        | AstNode.Expression(SynExpr.Paren(_)), HintExpr(_)
        | AstNode.Pattern(SynPat.Paren(_)), HintPat(_) -> ()
        | AstNode.Pattern(pattern), HintPat(hintPattern) ->
            if MatchPattern.matchHintPattern (pattern, hintPattern) then
                hintError hint visitorInfo pattern.Range
        | AstNode.Expression(expr), HintExpr(hintExpr) -> 
            let arguments =
                { MatchExpression.LambdaArguments = Map.ofList []
                  MatchExpression.Expression = node.Actual
                  MatchExpression.Hint = hintExpr
                  MatchExpression.FSharpCheckFileResults = checkFile
                  MatchExpression.Breadcrumbs = breadcrumbs }

            if MatchExpression.matchHintExpr arguments then
                match hint.Match, hint.Suggestion with
                | HintExpr(Expression.Lambda(_)), Suggestion.Expr(Expression.Identifier(_)) -> 
                    if lambdaCanBeReplacedWithFunction checkFile breadcrumbs expr.Range then
                        hintError hint visitorInfo expr.Range
                | _ ->
                    hintError hint visitorInfo expr.Range
        | _ -> ()

    let analyser getHints visitorInfo checkFile (syntaxArray:AbstractSyntaxArray.Node []) (skipArray:AbstractSyntaxArray.Skip []) = 
        let hintKeywordTree = getHints visitorInfo.Config

        let maxBreadcrumbs = 6

        let confirmFuzzyMatch i =
            let breadcrumbs = AbstractSyntaxArray.getBreadcrumbs maxBreadcrumbs syntaxArray skipArray i
            let isSuppressed =
                AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
                |> List.exists (List.exists (fun (l, _) -> l.Category = AnalyserName))
            if not isSuppressed then
                confirmFuzzyMatch visitorInfo checkFile syntaxArray.[i] breadcrumbs
            else
                ignore

        FuzzyHintMatcher.possibleMatches syntaxArray skipArray hintKeywordTree confirmFuzzyMatch

    let getHintsFromConfig config =
        let analyser = Map.find AnalyserName config.Analysers

        match Map.tryFind "Hints" analyser.Settings with
        | Some(Hints(hints)) -> 
            List.map (fun x -> x.ParsedHint) hints
            |> MergeSyntaxTrees.mergeHints
        | _ ->
            Debug.Assert(false, "Hints analyser was not in the configuration.")
            MergeSyntaxTrees.Edges.Empty