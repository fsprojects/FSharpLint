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
    open FSharpLint.Framework.LoadAnalysers

    [<Literal>]
    let AnalyserName = "FSharpLint.Hints"

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

    /// Converts an operator name e.g. op_Add to the operator symbol e.g. +
    let private identAsDecompiledOpName (ident:Ident) =
        if ident.idText.StartsWith("op_") then
            Microsoft.FSharp.Compiler.PrettyNaming.DecompileOpName ident.idText
        else 
            ident.idText

    let matchExpr = function
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
    let removeAutoGeneratedMatchesFromLambda numberOfWildcards =
        let rec removeMatchClauses wildcard = function
            | x when wildcard = numberOfWildcards -> Some(x)
            | SynExpr.Match(SequencePointInfoForBinding.NoSequencePointAtInvisibleBinding, 
                            _, 
                            [SynMatchClause.Clause(SynPat.Wild(_), _, expr, _, _)], _, _) ->
                removeMatchClauses (wildcard + 1) expr
            | _ -> None

        removeMatchClauses 0

    /// Extracts an expression from parentheses e.g. ((x + 4)) -> x + 4
    let rec removeParens = function
        | SynExpr.Paren(x, _, _, _) -> removeParens x
        | x -> x
        
    let flattenFunctionApplication expr =
        let listContains value list = List.exists ((=) value) list

        let isForwardPipeOperator op = ["|>";"||>";"|||>"] |> listContains op
        let isBackwardPipeOperator op = ["<|";"<||";"<|||"] |> listContains op

        let rec flattenFunctionApplication exprs = function
            | SynExpr.App(_, _, x, y, _) -> 
                match removeParens x with
                    | SynExpr.App(_, true, SynExpr.Ident(op), rightExpr, _) ->
                        let opIdent = identAsDecompiledOpName op

                        if isForwardPipeOperator opIdent then
                            let flattened = flattenFunctionApplication [] y
                            flattened@[rightExpr]
                        else if isBackwardPipeOperator opIdent then
                            let flattened = flattenFunctionApplication [] rightExpr
                            flattened@[y]
                        else
                            flattenFunctionApplication (y::exprs) x
                    | _ -> 
                        flattenFunctionApplication (y::exprs) x
            | x -> 
                x::exprs

        flattenFunctionApplication [] expr

    let rec matchHintExpr arguments (expr, hint) =
        let expr = removeParens expr

        match hint with
            | Expression.Variable(variable) when arguments |> Map.containsKey variable ->
                match expr with
                    | SynExpr.Ident(identifier) 
                        when identifier.idText = arguments.[variable] -> 
                            true
                    | _ -> false
            | Expression.Variable(_)
            | Expression.Wildcard ->
                true
            | Expression.Constant(_)
            | Expression.Identifier(_) ->
                matchExpr expr = Some(hint)
            | Expression.FunctionApplication(_) ->
                matchFunctionApplication arguments (expr, hint)
            | Expression.InfixOperator(_) ->
                matchInfixOperation arguments (expr, hint)
            | Expression.PrefixOperator(_) ->
                matchPrefixOperation arguments (expr, hint)
            | Expression.Parentheses(hint) -> 
                matchHintExpr arguments (expr, hint)
            | Expression.Lambda(_) -> 
                matchLambda arguments (expr, hint)

    and matchFunctionApplication arguments (expr, hint) =
        match (expr, hint) with
            | SynExpr.App(_) as application, Expression.FunctionApplication(hintExpressions) -> 
                let expressions = flattenFunctionApplication application

                List.length expressions = List.length hintExpressions &&
                    (expressions, hintExpressions)
                        ||> List.forall2 (fun x y -> matchHintExpr arguments (x, y))
            | _ -> false

    and matchLambda arguments (expr, hint) =
        match (expr, hint) with
            | SynExpr.Lambda(_) as lambda, Expression.Lambda(lambdaHint) -> 
                match matchLambdaArguments lambdaHint.Arguments lambda with
                    | LambdaMatch.Match(bodyExpr, arguments, numberOfWildcards) -> 
                        match removeAutoGeneratedMatchesFromLambda numberOfWildcards bodyExpr with
                            | Some(bodyExpr) -> 
                                matchHintExpr arguments (bodyExpr, lambdaHint.Body)
                            | None -> false
                    | LambdaMatch.NoMatch -> false
            | _ -> false

    and matchInfixOperation arguments (expr, hint) =
        match (expr, hint) with
            | SynExpr.App(_, _, infixExpr, leftExpr, _) as application, 
                    Expression.InfixOperator(op, left, right) -> 

                match removeParens infixExpr with
                    | SynExpr.App(_, true, opExpr, rightExpr, _) ->
                        matchHintExpr arguments (opExpr, Expression.Identifier([op])) &&
                        matchHintExpr arguments (rightExpr, left) &&
                        matchHintExpr arguments (leftExpr, right)
                    | _ -> false
            | _ -> false

    and matchPrefixOperation arguments (expr, hint) =
        match (expr, hint) with
            | SynExpr.App(_, _, opExpr, rightExpr, _) as application, 
                    Expression.PrefixOperator(op, expr) -> 
                matchHintExpr arguments (opExpr, Expression.Identifier(["~" + op])) &&
                matchHintExpr arguments (rightExpr, expr)
            | SynExpr.AddressOf(_, addrExpr, _, _), Expression.PrefixOperator(op, expr) 
                    when op = "&" || op = "&&" ->
                matchHintExpr arguments (addrExpr, expr)
            | _ -> false

    /// Gets a list of hints from the config file.
    let getHints config = 
        let analyser = Map.find AnalyserName config

        let hintRule = Map.find "Hints" analyser.Rules

        let parseHint hint =
            match run phint hint with
                | Success(hint, _, _) -> hint
                | Failure(error, _, _) -> 
                    raise <| ConfigurationException("Failed to parse hint: " + hint + "\n" + error)

        match Map.find "Hints" hintRule.Settings with
            | Hints(stringHints) -> 
                stringHints 
                    |> List.filter (fun x -> not <| System.String.IsNullOrEmpty(x))
                    |> List.map parseHint
            | _ -> 
                raise <| ConfigurationException("Expected the Hints rule to contain a list of hints")

    /// Memoized getHints.
    let getHintsFromConfig = 
        let hints = ref None

        fun (config:Map<string,Analyser>) ->
            match !hints with
                | None ->
                    let parsedHints = getHints config
                    hints := Some(parsedHints)
                    parsedHints
                | Some(hints) -> hints

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

    let rec hintToString = function
        | Expression.Variable(x) -> x.ToString()
        | Expression.Wildcard -> "_"
        | Expression.Constant(constant) -> 
            constantToString constant
        | Expression.Identifier(identifier) ->
            String.concat "." identifier
        | Expression.FunctionApplication(hints) ->
            hints 
                |> List.map hintToString
                |> String.concat " "
        | Expression.InfixOperator(operator, leftHint, rightHint) ->
            hintToString leftHint + operator + hintToString rightHint
        | Expression.PrefixOperator(operator, hint) ->
            operator + hintToString hint
        | Expression.Parentheses(hint) -> 
            "(" + hintToString hint + ")"
        | Expression.Lambda(lambda) -> 
            "fun " + lambdaArgumentsToString lambda.Arguments + " -> " + hintToString lambda.Body

    let visitor getHints visitorInfo (checkFile:CheckFileResults) astNode = 
        if isAnalyserEnabled visitorInfo.Config then
            match astNode.Node with
                | AstNode.Expression(expr) -> 
                    for hint in getHints visitorInfo.Config do
                        if matchHintExpr (Map.ofList []) (expr, hint.Match) then
                            let matched = hintToString hint.Match
                            let suggestion = hintToString hint.Suggestion

                            let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesHintRefactor")
                            let error = System.String.Format(errorFormatString, matched, suggestion)

                            visitorInfo.PostError expr.Range error

                    Continue
                | _ -> Continue
        else
            Stop

    type RegisterHintAnalyser() = 
        let plugin =
            {
                Name = AnalyserName
                Analyser = Ast(visitor getHintsFromConfig)
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