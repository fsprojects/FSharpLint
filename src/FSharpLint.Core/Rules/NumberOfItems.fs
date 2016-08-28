// FSharpLint, a linter for F#.
// Copyright (C) 2016 Matthew Mcveigh
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace FSharpLint.Rules

/// Checks if a function is declared with more than a configurable number of parameters.
module NumberOfItems =
    
    open System
    open Microsoft.FSharp.Compiler.Ast
    open FSharpLint.Framework
    open FSharpLint.Framework.Analyser
    open FSharpLint.Framework.Ast

    type NumberOfItemsRule(analyserName, name, code, ruleConfig) =
        inherit Rule(analyserName, name, code, ruleConfig)

        member __.MaxItems =
            match Configuration.isRuleEnabled ruleConfig analyserName name with
            | Some(_, ruleSettings) ->
                match Map.tryFind "MaxItems" ruleSettings with
                | Some(Configuration.MaxItems(i)) -> Some(i)
                | Some(_) | None -> None
            | Some(_) | None -> None

    module Analysis =
        type INumberOfItemsAnalyser =
            abstract member MaxNumberOfItemsInTuple: NumberOfItemsRule
            abstract member MaxNumberOfFunctionParameters: NumberOfItemsRule
            abstract member MaxNumberOfMembers: NumberOfItemsRule
            abstract member MaxNumberOfBooleanOperatorsInCondition: NumberOfItemsRule

        let private validateTuple (items:SynExpr list) analysisArgs (analyser:INumberOfItemsAnalyser) i =
            let rule = analyser.MaxNumberOfItemsInTuple

            rule.MaxItems |> Option.iter (fun maxItems ->
                if List.length items > maxItems && rule.NotSuppressed analysisArgs i then
                    let message = rule.MessageFormat(maxItems)
                    analysisArgs.Context.PostError (items.[maxItems].Range) message)

        let private validateFunction (constructorArguments:SynConstructorArgs) analysisArgs (analyser:INumberOfItemsAnalyser) i = 
            let rule = analyser.MaxNumberOfFunctionParameters

            let checkNumberOfParameters maxParameters =
                match constructorArguments with
                | SynConstructorArgs.Pats(parameters) 
                        when List.length parameters > maxParameters && rule.NotSuppressed analysisArgs i -> 
                    let message = rule.MessageFormat(maxParameters)
                    analysisArgs.Context.PostError parameters.[maxParameters].Range message
                | _ -> ()

            rule.MaxItems |> Option.iter checkNumberOfParameters

        let private getMembers (members:SynMemberDefn list) =
            let isPublic = function
                | Some(SynAccess.Public) | None -> true
                | Some(_) -> false

            let isPublicMember = function
                | SynMemberDefn.AbstractSlot(_) -> true
                | SynMemberDefn.Member(SynBinding.Binding(access, _, _, _, _, _, _, _, _, _, _, _), _)
                | SynMemberDefn.AutoProperty(_, _, _, _, _, _, _, access, _, _, _) -> isPublic access
                | _ -> false

            members |> List.filter isPublicMember

        let private validateType members typeRepresentation analysisArgs (analyser:INumberOfItemsAnalyser) i =
            let members = 
                match typeRepresentation with
                | SynTypeDefnRepr.Simple(_) | SynTypeDefnRepr.Exception(_) -> members
                | SynTypeDefnRepr.ObjectModel(_, members, _) -> members
                |> getMembers

            let rule = analyser.MaxNumberOfMembers
                                                        
            rule.MaxItems |> Option.iter (fun maxMembers ->
                if List.length members > maxMembers && rule.NotSuppressed analysisArgs i then
                    let message = rule.MessageFormat(maxMembers)
                    analysisArgs.Context.PostError (members.[maxMembers].Range) message)

        let private isInApplication (syntaxArray:AbstractSyntaxArray.Node[]) (skipArray:AbstractSyntaxArray.Skip[]) i =
            let rec isApplicationNode i = 
                if i <= 0 then false
                else
                    match syntaxArray.[i].Actual with
                    | AstNode.Expression(SynExpr.Paren(_)) -> isApplicationNode skipArray.[i].ParentIndex
                    | AstNode.Expression(SynExpr.App(_) | SynExpr.New(_)) -> true
                    | _ -> false

            if i <= 0 then false
            else isApplicationNode skipArray.[i].ParentIndex

        let private validateCondition condition analysisArgs (analyser:INumberOfItemsAnalyser) i =
            let rec countBooleanOperators total = function
                | SynExpr.App(_, _, expr, SynExpr.Ident(ident), _)
                | SynExpr.App(_, _, SynExpr.Ident(ident), expr, _) -> 
                    if List.exists ((=) ident.idText) ["op_BooleanOr"; "op_BooleanAnd"; "not"] then
                        countBooleanOperators (total + 1) expr
                    else
                        countBooleanOperators total expr
                | SynExpr.App(_, _, expr, expr2, _) ->
                    total + countBooleanOperators 0 expr + countBooleanOperators 0 expr2
                | SynExpr.Paren(expr, _, _, _) ->
                    countBooleanOperators total expr
                | _ -> total

            let rule = analyser.MaxNumberOfBooleanOperatorsInCondition

            let checkNumberOfBooleanOperatorsInCondition maxBooleanOperators =
                let numberOfBooleanOperators = countBooleanOperators 0 condition

                if numberOfBooleanOperators > maxBooleanOperators && rule.NotSuppressed analysisArgs i then
                    let message = rule.MessageFormat(maxBooleanOperators)
                    analysisArgs.Context.PostError condition.Range message

            rule.MaxItems |> Option.iter checkNumberOfBooleanOperatorsInCondition
    
        let analyse (analyser:INumberOfItemsAnalyser) analysisArgs = 
            let mutable i = 0
            while i < analysisArgs.SyntaxArray.Length do
                match analysisArgs.SyntaxArray.[i].Actual with
                | AstNode.Pattern(SynPat.LongIdent(_, _, _, constructorArguments, _, _)) ->
                    validateFunction constructorArguments analysisArgs analyser i
                | AstNode.Expression(expression) ->
                    match expression with
                    | SynExpr.Tuple(expressions, _, _) ->
                        if not <| isInApplication analysisArgs.SyntaxArray analysisArgs.SkipArray i then
                            validateTuple expressions analysisArgs analyser i
                    | SynExpr.IfThenElse(condition, _, _, _, _, _, _)
                    | SynExpr.While(_, condition, _, _)
                    | SynExpr.Assert(condition, _) ->
                        validateCondition condition analysisArgs analyser i
                    | _ -> ()
                | AstNode.Match(SynMatchClause.Clause(_, Some(whenExpr), _, _, _)) ->
                    validateCondition whenExpr analysisArgs analyser i
                | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(_, typeRepresentation, members, _)) ->
                    validateType members typeRepresentation analysisArgs analyser i
                | _ -> ()
            
                i <- i + 1

    [<Sealed>]
    type NumberOfItemsAnalyser(config) =
        inherit Analyser.Analyser(name = "NumberOfItems", code = "2", config = config)

        interface Analysis.INumberOfItemsAnalyser with
            member this.MaxNumberOfItemsInTuple = 
                NumberOfItemsRule(this.Name, "MaxNumberOfItemsInTuple", "1", config)

            member this.MaxNumberOfFunctionParameters = 
                NumberOfItemsRule(this.Name, "MaxNumberOfFunctionParameters", "2", config)

            member this.MaxNumberOfMembers = 
                NumberOfItemsRule(this.Name, "MaxNumberOfMembers", "3", config)

            member this.MaxNumberOfBooleanOperatorsInCondition = 
                NumberOfItemsRule(this.Name, "MaxNumberOfBooleanOperatorsInCondition", "4", config)

        override this.Analyse analysisArgs = 
            Analysis.analyse this analysisArgs