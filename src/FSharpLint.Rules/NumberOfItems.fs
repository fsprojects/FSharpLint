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

/// Checks if a function is declared with more than a configurable number of parameters.
module NumberOfItems =
    
    open System
    open Microsoft.FSharp.Compiler.Ast
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadVisitors

    [<Literal>]
    let AnalyserName = "NumberOfItems"

    let maxItemsForRule config (astNode:CurrentNode) ruleName =
        match isRuleEnabled config AnalyserName ruleName with
        | Some(_, ruleSettings) 
                when ruleSettings.ContainsKey "MaxItems" && astNode.IsSuppressed(AnalyserName, ruleName) |> not ->
            match ruleSettings.["MaxItems"] with
            | MaxItems(i) -> Some(i)
            | _ -> None
        | Some(_) | None -> None

    let validateTuple (items:SynExpr list) visitorInfo astNode =
        maxItemsForRule visitorInfo.Config astNode "MaxNumberOfItemsInTuple"
        |> Option.iter (fun maxItems ->
            if List.length items > maxItems then
                let errorFormatString = Resources.GetString("RulesNumberOfItemsTupleError")
                let error = String.Format(errorFormatString, maxItems)
                visitorInfo.PostError (items.[maxItems].Range) error)

    let validateFunction (constructorArguments:SynConstructorArgs) visitorInfo astNode = 
        let checkNumberOfParameters maxParameters =
            match constructorArguments with
            | SynConstructorArgs.Pats(parameters) when List.length parameters > maxParameters -> 
                let errorFormatString = Resources.GetString("RulesNumberOfItemsFunctionError")
                let error = String.Format(errorFormatString, maxParameters)
                visitorInfo.PostError parameters.[maxParameters].Range error
            | _ -> ()

        maxItemsForRule visitorInfo.Config astNode "MaxNumberOfFunctionParameters"
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

    let validateType members typeRepresentation visitorInfo astNode =
        let members = 
            match typeRepresentation with
            | SynTypeDefnRepr.Simple(_) -> members
            | SynTypeDefnRepr.ObjectModel(_, members, _) -> members
            |> getMembers
                                                        
        maxItemsForRule visitorInfo.Config astNode "MaxNumberOfMembers"
        |> Option.iter (fun maxMembers ->
            if List.length members > maxMembers then
                let errorFormatString = Resources.GetString("RulesNumberOfItemsClassMembersError")
                let error = String.Format(errorFormatString, maxMembers)
                visitorInfo.PostError (members.[maxMembers].Range) error)

    let isInApplication astNode =
        let rec getApplicationNode = function
            | node :: parents ->
                match node with
                | AstNode.Expression(SynExpr.Paren(_)) -> getApplicationNode parents
                | AstNode.Expression(SynExpr.App(_) as application)
                | AstNode.Expression(SynExpr.New(_) as application) -> Some(application)
                | _ -> None
            | [] -> None

        match getApplicationNode astNode.Breadcrumbs with
        | Some(SynExpr.App(_)) | Some(SynExpr.New(_)) -> true
        | Some(_) | None -> false

    let validateCondition condition visitorInfo astNode =
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

        let checkNumberOfBooleanOperatorsInCondition maxBooleanOperators =
            let numberOfBooleanOperators = countBooleanOperators 0 condition

            if numberOfBooleanOperators > maxBooleanOperators then
                let errorFormatString = Resources.GetString("RulesNumberOfItemsBooleanConditionsError")
                let error = String.Format(errorFormatString, maxBooleanOperators)
                visitorInfo.PostError condition.Range error

        maxItemsForRule visitorInfo.Config astNode "MaxNumberOfBooleanOperatorsInCondition"
        |> Option.iter checkNumberOfBooleanOperatorsInCondition
    
    let visitor visitorInfo _ astNode = 
        match astNode.Node with
        | AstNode.Pattern(SynPat.LongIdent(_, _, _, constructorArguments, _, _)) ->
            validateFunction constructorArguments visitorInfo astNode
        | AstNode.Expression(expression) ->
            match expression with
            | SynExpr.Tuple(expressions, _, _) ->
                if not <| isInApplication astNode then
                    validateTuple expressions visitorInfo astNode
            | SynExpr.IfThenElse(condition, _, _, _, _, _, _)
            | SynExpr.While(_, condition, _, _)
            | SynExpr.Assert(condition, _) ->
                validateCondition condition visitorInfo astNode
            | _ -> ()
        | AstNode.Match(SynMatchClause.Clause(_, Some(whenExpr), _, _, _)) ->
            validateCondition whenExpr visitorInfo astNode
        | AstNode.TypeDefinition(SynTypeDefn.TypeDefn(_, typeRepresentation, members, _)) ->
            validateType members typeRepresentation visitorInfo astNode
        | _ -> ()

        Continue

    type RegisterNumberOfItemsVisitor() = 
        let plugin =
            { Name = AnalyserName
              Visitor = Ast(visitor) }

        interface IRegisterPlugin with
            member __.RegisterPlugin = plugin