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
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadAnalysers

    [<Literal>]
    let AnalyserName = "FSharpLint.NumberOfItems"

    let maxItemsForRule (config:Map<string,Analyser>) ruleName =
        match isRuleEnabled config AnalyserName ruleName with
            | Some(_, ruleSettings) when ruleSettings.ContainsKey "MaxItems" ->
                match ruleSettings.["MaxItems"] with
                    | MaxItems(i) -> Some(i)
                    | _ -> None
            | Some(_)
            | None -> None

    let validateTuple (items:SynExpr list) visitorInfo =
        maxItemsForRule visitorInfo.Config "MaxNumberOfItemsInTuple"
            |> Option.iter (fun maxItems ->
                if List.length items > maxItems then
                    let error = sprintf "Tuple should have a maximum of %d items" maxItems
                    visitorInfo.PostError (items.[maxItems].Range) error)

    let validateFunction (constructorArguments:SynConstructorArgs) visitorInfo = 
        maxItemsForRule visitorInfo.Config "MaxNumberOfFunctionParameters"
            |> Option.iter (fun maxParameters ->
                match constructorArguments with
                    | SynConstructorArgs.Pats(parameters) when List.length parameters > maxParameters -> 
                        let error = sprintf "Functions should have a maximum of %d parameters" maxParameters 
                        visitorInfo.PostError parameters.[maxParameters].Range error
                    | _ -> ())

    let private getMembers (members:SynMemberDefn list) =
        let isPublic = function
            | Some(access) when access = SynAccess.Public -> true
            | Some(_) -> false
            | None -> true

        let isPublicMember = function
            | SynMemberDefn.AbstractSlot(_) ->
                true
            | SynMemberDefn.Member(SynBinding.Binding(access, _, _, _, _, _, _, _, _, _, _, _), _)
            | SynMemberDefn.AutoProperty(_, _, _, _, _, _, _, access, _, _, _) ->
                isPublic access
            | _ -> false

        members 
            |> List.filter isPublicMember

    let validateType members typeRepresentation visitorInfo =
        let members = 
            getMembers <|
                match typeRepresentation with
                    | SynTypeDefnRepr.Simple(_) -> members
                    | SynTypeDefnRepr.ObjectModel(_, members, _) -> members
                                                        
        maxItemsForRule visitorInfo.Config "MaxNumberOfMembers"
            |> Option.iter (fun maxMembers ->
                if List.length members > maxMembers then
                    let error = sprintf "Class must have a maximum of %d members" maxMembers
                    visitorInfo.PostError (members.[maxMembers].Range) error)

    let isTupleAppliedToMember astNode =
        let rec getApplicationNode = function
            | node :: parents ->
                match node with
                    | AstNode.Expression(SynExpr.Paren(_)) ->
                        getApplicationNode parents
                    | AstNode.Expression(SynExpr.App(_) as application)
                    | AstNode.Expression(SynExpr.New(_) as application) ->
                        Some(application)
                    | _ -> 
                        None
            | [] -> 
                None

        match astNode.Breadcrumbs |> getApplicationNode with
            | Some(SynExpr.App(flag, _, _, _, _)) when flag = ExprAtomicFlag.Atomic -> true
            | Some(SynExpr.New(_)) -> true
            | Some(_)
            | None -> false
    
    let visitor visitorInfo checkFile astNode = 
        match astNode.Node with
            | AstNode.Pattern(pattern) ->
                match pattern with
                    | SynPat.LongIdent(_, _, _, constructorArguments, _, _) ->
                        validateFunction constructorArguments visitorInfo
                    | _ -> ()
            | AstNode.Expression(expression) ->
                match expression with
                    | SynExpr.Tuple(expressions, _, _) ->
                        if not <| isTupleAppliedToMember astNode then
                            validateTuple expressions visitorInfo
                    | _ -> ()
            | AstNode.TypeDefinition(typeDefinition) ->
                match typeDefinition with
                    | SynTypeDefn.TypeDefn(_, typeRepresentation, members, _) ->
                        validateType members typeRepresentation visitorInfo
            | _ -> ()

        Continue

    type RegisterNumberOfItemsAnalyser() = 
        let plugin =
            {
                Name = AnalyserName
                Analyser = Ast(visitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin