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
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration

    [<Literal>]
    let AnalyserName = "NumberOfItems"
    
    let private maxItemsForRule config ruleName =
        match isRuleEnabled config AnalyserName ruleName with
        | Some(_, ruleSettings) ->
            match Map.tryFind "MaxItems" ruleSettings with
            | Some(MaxItems(i)) -> Some(i)
            | Some(_) | None -> None
        | Some(_) | None -> None

    let private validateTuple (items:SynExpr list) visitorInfo isSuppressed =
        let ruleName = "MaxNumberOfItemsInTuple"

        maxItemsForRule visitorInfo.Config ruleName
        |> Option.iter (fun maxItems ->
            if List.length items > maxItems && not <| isSuppressed ruleName then
                let errorFormatString = Resources.GetString("RulesNumberOfItemsTupleError")
                let error = String.Format(errorFormatString, maxItems)
                visitorInfo.PostError (items.[maxItems].Range) error)

    let private validateFunction (constructorArguments:SynConstructorArgs) visitorInfo isSuppressed = 
        let ruleName = "MaxNumberOfFunctionParameters"

        let checkNumberOfParameters maxParameters =
            match constructorArguments with
            | SynConstructorArgs.Pats(parameters) 
                    when List.length parameters > maxParameters && not <| isSuppressed ruleName -> 
                let errorFormatString = Resources.GetString("RulesNumberOfItemsFunctionError")
                let error = String.Format(errorFormatString, maxParameters)
                visitorInfo.PostError parameters.[maxParameters].Range error
            | _ -> ()

        maxItemsForRule visitorInfo.Config ruleName
        |> Option.iter checkNumberOfParameters

    let private getMembers (members:SynMemberDefn list) =
        let isPublic = function
            | Some(SynAccess.Public) | None -> true
            | Some(_) -> false

        let isPublicMember = function
            | SynMemberDefn.AbstractSlot(_) -> true
            | SynMemberDefn.Member(SynBinding.Binding(access, _, _, _, _, _, _, _, _, _, _, _), _)
            | SynMemberDefn.AutoProperty(_, _, _, _, _, _, _, access, _, _, _) -> isPublic access
            | _ -> false

        members 
        |> List.filter isPublicMember

    let private validateType members typeRepresentation visitorInfo isSuppressed =
        let members = 
            match typeRepresentation with
            | SynTypeDefnRepr.Simple(_) -> members
            | SynTypeDefnRepr.ObjectModel(_, members, _) -> members
            |> getMembers

        let ruleName = "MaxNumberOfMembers"
                                                        
        maxItemsForRule visitorInfo.Config ruleName
        |> Option.iter (fun maxMembers ->
            if List.length members > maxMembers && not <| isSuppressed ruleName then
                let errorFormatString = Resources.GetString("RulesNumberOfItemsClassMembersError")
                let error = String.Format(errorFormatString, maxMembers)
                visitorInfo.PostError (members.[maxMembers].Range) error)

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

    let private validateCondition condition visitorInfo isSuppressed =
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

        let ruleName = "MaxNumberOfBooleanOperatorsInCondition"

        let checkNumberOfBooleanOperatorsInCondition maxBooleanOperators =
            let numberOfBooleanOperators = countBooleanOperators 0 condition

            if numberOfBooleanOperators > maxBooleanOperators && not <| isSuppressed ruleName then
                let errorFormatString = Resources.GetString("RulesNumberOfItemsBooleanConditionsError")
                let error = String.Format(errorFormatString, maxBooleanOperators)
                visitorInfo.PostError condition.Range error

        maxItemsForRule visitorInfo.Config ruleName
        |> Option.iter checkNumberOfBooleanOperatorsInCondition
    
    let analyser visitorInfo _ syntaxArray skipArray = 
        let isSuppressed i ruleName = 
            AbstractSyntaxArray.getSuppressMessageAttributes syntaxArray skipArray i 
            |> AbstractSyntaxArray.isRuleSuppressed AnalyserName ruleName

        let mutable i = 0
        while i < syntaxArray.Length do
            match syntaxArray.[i].Actual with
            | AstNode.Pattern(SynPat.LongIdent(_, _, _, constructorArguments, _, _)) ->
                validateFunction constructorArguments visitorInfo (isSuppressed i)
            | AstNode.Expression(expression) ->
                match expression with
                | SynExpr.Tuple(expressions, _, _) ->
                    if not <| isInApplication syntaxArray skipArray i then
                        validateTuple expressions visitorInfo (isSuppressed i)
                | SynExpr.IfThenElse(condition, _, _, _, _, _, _)
                | SynExpr.While(_, condition, _, _)
                | SynExpr.Assert(condition, _) ->
                    validateCondition condition visitorInfo (isSuppressed i)
                | _ -> ()
            | AstNode.Match(SynMatchClause.Clause(_, Some(whenExpr), _, _, _)) ->
                validateCondition whenExpr visitorInfo (isSuppressed i)
            | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(_, typeRepresentation, members, _)) ->
                validateType members typeRepresentation visitorInfo (isSuppressed i)
            | _ -> ()
            
            i <- i + 1