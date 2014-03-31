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

module Ast =

    open System
    open System.Text.RegularExpressions
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Tokeniser

    type VisitorInfo =
        {
            Config: Map<string, Configuration.Analyser>
            PostError: range -> string -> unit
        }

    type AstNode =
        | Expression of SynExpr
        | Pattern of SynPat
        | SimplePattern of SynSimplePat
        | SimplePatterns of SynSimplePats
        | ModuleOrNamespace of SynModuleOrNamespace
        | ModuleDeclaration of SynModuleDecl
        | Binding of SynBinding
        | TypeDefinition of SynTypeDefn
        | MemberDefinition of SynMemberDefn
        | ComponentInfo of SynComponentInfo
        | ExceptionDefinition of SynExceptionDefn
        | ExceptionRepresentation of SynExceptionRepr
        | UnionCase of SynUnionCase
        | EnumCase of SynEnumCase
        | TypeRepresentation of SynTypeDefnRepr
        | TypeSimpleRepresentation of SynTypeDefnSimpleRepr
        | Type of SynType
        | Field of SynField
        | Match of SynMatchClause
        | ConstructorArguments of SynConstructorArgs
        | TypeParameter of SynTypar
        | InterfaceImplementation of SynInterfaceImpl
        
    let isPublic path =
        let isSynAccessPublic = function
            | Some(SynAccess.Public) -> true
            | None -> true
            | _ -> false

        let rec isPublic publicSoFar isBinding = function
            | node :: path when publicSoFar ->
                match node with
                    | Pattern(pattern) ->
                        match pattern with
                            | SynPat.Named(_, _, _, access, _)
                            | SynPat.LongIdent(_, _, _, _, access, _) ->
                                isPublic (isSynAccessPublic access) isBinding path
                            | _ -> true
                    | Binding(binding) ->
                        match binding with
                            | SynBinding.Binding(access, _, _, _, _, _, _, _, _, _, _, _) ->
                                isPublic (isSynAccessPublic access) true path
                    | TypeSimpleRepresentation(typeSimpleRepresentation) ->
                        match typeSimpleRepresentation with
                            | SynTypeDefnSimpleRepr.Record(access, _, _)
                            | SynTypeDefnSimpleRepr.Union(access, _, _) -> 
                                isPublic (isSynAccessPublic access) isBinding path
                            | _ -> true
                    | UnionCase(unionCase) ->
                        match unionCase with
                            | SynUnionCase.UnionCase(_, _, _, _, access, _) -> 
                                isPublic (isSynAccessPublic access) isBinding path
                    | Field(field) ->
                        match field with
                            | SynField.Field(_, _, _, _, _, _, access, _) ->
                                isPublic (isSynAccessPublic access) isBinding path
                    | ComponentInfo(componentInfo) ->
                        match componentInfo with
                            | SynComponentInfo.ComponentInfo(_, _, _, _, _, _, access, _) ->
                                isPublic (isSynAccessPublic access) isBinding path
                    | MemberDefinition(memberDefinition) ->
                        match memberDefinition with
                        | SynMemberDefn.NestedType(_, access, _)
                        | SynMemberDefn.AutoProperty(_, _, _, _, _, _, _, access, _, _, _)
                        | SynMemberDefn.ImplicitCtor(access, _, _, _, _)
                        | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(_, _, _, _, _, _, _, _, access, _, _), _, _) ->
                                isPublic (isSynAccessPublic access) isBinding path
                        | _ -> isPublic publicSoFar isBinding path
                    | ExceptionRepresentation(exceptionRepresentation) ->
                        match exceptionRepresentation with
                            | SynExceptionRepr.ExceptionDefnRepr(_, _, _, _, access, _) ->
                                isPublic (isSynAccessPublic access) isBinding path
                    | ModuleOrNamespace (moduleOrNamespace) ->
                        match moduleOrNamespace with
                            | SynModuleOrNamespace.SynModuleOrNamespace(_, _, _, _, _, access, _) ->
                                isPublic (isSynAccessPublic access) isBinding path
                    | ExceptionDefinition(_)
                    | EnumCase(_)
                    | TypeRepresentation(_)
                    | Type(_)
                    | Match(_)
                    | ConstructorArguments(_)
                    | TypeParameter(_)
                    | InterfaceImplementation(_)
                    | ModuleDeclaration(_)
                    | SimplePattern(_)
                    | SimplePatterns(_) -> isPublic publicSoFar isBinding  path
                    | TypeDefinition(_) -> 
                        if isBinding then
                            false
                        else
                            isPublic publicSoFar isBinding path
                    | Expression(_) ->
                        if isBinding then
                            false
                        else
                            isPublic publicSoFar isBinding path
            | [] -> publicSoFar
            | _ -> false

        isPublic true false path

    let traverseNode node =
        [
            match node with
                | ModuleDeclaration(moduleDeclaration) ->
                    match moduleDeclaration with
                        | SynModuleDecl.NestedModule(componentInfo, moduleDeclarations, _, _) ->
                            yield ComponentInfo(componentInfo)
                            for x in moduleDeclarations do yield ModuleDeclaration(x)
                        | SynModuleDecl.Let(_, bindings, _) -> 
                            for x in bindings do yield Binding(x)
                        | SynModuleDecl.DoExpr(_, expression, _) -> 
                            yield Expression(expression)
                        | SynModuleDecl.Types(typeDefinitions, _) -> 
                            for x in typeDefinitions do yield TypeDefinition(x)
                        | SynModuleDecl.Exception(exceptionDefinition, _) -> 
                            yield ExceptionDefinition(exceptionDefinition)
                        | SynModuleDecl.NamespaceFragment(moduleOrNamespace) -> 
                            yield ModuleOrNamespace(moduleOrNamespace)
                        | SynModuleDecl.Open(_, _)
                        | SynModuleDecl.Attributes(_, _)
                        | SynModuleDecl.HashDirective(_, _)
                        | SynModuleDecl.ModuleAbbrev(_, _, _) -> ()
                | ModuleOrNamespace(moduleOrNamespace) ->
                    match moduleOrNamespace with
                        | SynModuleOrNamespace(_, _, moduleDeclarations, _, _, _, _) ->
                            for x in moduleDeclarations do yield ModuleDeclaration(x)
                | Binding(binding) ->
                    match binding with
                        | SynBinding.Binding(_, _, _, _, _, _, _, pattern, _, expression, _, _) ->
                            yield Pattern(pattern)
                            yield Expression(expression)
                | ExceptionDefinition(exceptionDefinition) ->
                    match exceptionDefinition with
                        | SynExceptionDefn.ExceptionDefn(exceptionRepresentation, members, _) -> 
                            yield ExceptionRepresentation(exceptionRepresentation)
                            for x in members do yield MemberDefinition(x)
                | ExceptionRepresentation(exceptionRepresentation) ->
                    match exceptionRepresentation with
                        | SynExceptionRepr.ExceptionDefnRepr(_, unionCase, _, _, _, _) -> 
                            yield UnionCase(unionCase)
                | TypeDefinition(typeDefinition) ->
                    match typeDefinition with
                        | SynTypeDefn.TypeDefn(componentInfo, typeRepresentation, members, _) -> 
                            yield ComponentInfo(componentInfo)
                            yield TypeRepresentation(typeRepresentation)
                            for x in members do yield MemberDefinition(x)
                | ComponentInfo(componentInfo) ->
                    match componentInfo with
                        | SynComponentInfo.ComponentInfo(_, _, _, _, _, _, _, _) -> ()
                | TypeRepresentation(typeRepresentation) ->
                    match typeRepresentation with
                        | ObjectModel(typeKind, members, _) -> 
                            for x in members do yield MemberDefinition(x)
                        | Simple(typeSimpleRepresentation, _) -> 
                            yield TypeSimpleRepresentation(typeSimpleRepresentation)
                | TypeSimpleRepresentation(typeSimpleRepresentation) ->
                    match typeSimpleRepresentation with
                        | SynTypeDefnSimpleRepr.Union(access, unionCases, range) -> 
                            for x in unionCases do yield UnionCase(x)
                        | SynTypeDefnSimpleRepr.Enum(enumCases, _) -> 
                            for x in enumCases do yield EnumCase(x)
                        | SynTypeDefnSimpleRepr.Record(_, fields, _) -> 
                            for x in fields do yield Field(x)
                        | SynTypeDefnSimpleRepr.TypeAbbrev(_, synType, _) -> 
                            yield Type(synType)
                        | SynTypeDefnSimpleRepr.General(_, _, _, _, _, _, _, _)
                        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly(_, _)
                        | SynTypeDefnSimpleRepr.None(_) -> ()
                | UnionCase(unionCase) ->
                    match unionCase with
                        | SynUnionCase.UnionCase(_, _, _, _, _, _) -> ()
                | EnumCase(enumCase) ->
                    match enumCase with
                        | SynEnumCase.EnumCase(_, _, _, _, _) -> ()
                | Field(field) ->
                    match field with
                        | SynField.Field(_, _, _, synType, _, _, _, _) -> 
                            yield Type(synType)
                | Type(synType) ->
                    match synType with
                        | SynType.App(synType, _, types, _, _, _, _) 
                        | SynType.LongIdentApp(synType, _, _, types, _, _, _) -> 
                            yield Type(synType)
                            for x in types do yield Type(x)
                        | SynType.Tuple(types, _) ->    
                            for (_, x) in types do yield Type(x)
                        | SynType.Fun(synType, synType1, _)
                        | SynType.StaticConstantNamed(synType, synType1, _)
                        | SynType.MeasureDivide(synType, synType1, _) -> 
                            yield Type(synType)
                            yield Type(synType1)
                        | SynType.WithGlobalConstraints(synType, _, _)
                        | SynType.HashConstraint(synType, _)
                        | SynType.MeasurePower(synType, _, _)
                        | SynType.Array(_, synType, _) -> 
                            yield Type(synType)
                        | SynType.StaticConstantExpr(expression, _) -> 
                            yield Expression(expression)
                        | SynType.Var(_, _)
                        | SynType.Anon(_)
                        | SynType.StaticConstant(_, _) 
                        | SynType.LongIdent(_) -> ()
                | Match(matchClause) ->
                    match matchClause with
                        | SynMatchClause.Clause(pattern, expression, expression1, _, _) -> 
                            yield Pattern(pattern)
                            match expression with 
                                | Some(x) -> yield Expression(x)
                                | None -> ()
                            yield Expression(expression1)
                | MemberDefinition(synMember) ->
                    match synMember with
                        | SynMemberDefn.Member(binding, _) -> 
                            yield Binding(binding)
                        | SynMemberDefn.ImplicitCtor(_, attributes, patterns, identifier, _) -> 
                            for x in patterns do yield SimplePattern(x)
                        | SynMemberDefn.ImplicitInherit(synType, expression, _, _) -> 
                            yield Type(synType)
                            yield Expression(expression)
                        | SynMemberDefn.LetBindings(bindings, _, _, _) -> 
                            for x in bindings do yield Binding(x)
                        | SynMemberDefn.Interface(synType, members, _) -> 
                            yield Type(synType)
                            match members with
                                | Some(members) -> for x in members do yield MemberDefinition(x)
                                | None -> ()
                        | SynMemberDefn.Inherit(synType, _, _) -> 
                            yield Type(synType)
                        | SynMemberDefn.ValField(field, _) -> 
                            yield Field(field)
                        | SynMemberDefn.NestedType(typeDefinition, _, _) -> 
                            yield TypeDefinition(typeDefinition)
                        | SynMemberDefn.AutoProperty(attributes, _, identifier, synType, _, _, _, _, expression, _, _) -> 
                            match synType with
                                | Some(synType) -> yield Type(synType)
                                | None -> ()
                            yield Expression(expression)
                        | SynMemberDefn.Open(_, _)
                        | SynMemberDefn.AbstractSlot(_, _, _) -> ()
                | Expression(expression) ->
                    match expression with
                        | SynExpr.Paren(expression, _, _, _) -> 
                            yield Expression(expression)
                        | SynExpr.Quote(expression, _, expression1, _, _)
                        | SynExpr.App(_, _, expression, expression1, _)
                        | SynExpr.Sequential(_, _, expression, expression1, _)
                        | SynExpr.NamedIndexedPropertySet(_, expression, expression1, _)
                        | SynExpr.DotIndexedSet(expression, _, expression1, _, _, _)
                        | SynExpr.JoinIn(expression, _, expression1, _)
                        | SynExpr.While(_, expression, expression1, _) -> 
                            yield Expression(expression)
                            yield Expression(expression1)
                        | SynExpr.Typed(expression, synType, _) -> 
                            yield Expression(expression)
                            yield Type(synType)
                        | SynExpr.Tuple(expressions, _, _)
                        | SynExpr.ArrayOrList(_, expressions, _) ->
                            for x in expressions do yield Expression(x)
                        | SynExpr.Record(synType, expression, _, _) -> 
                            match expression with
                                | Some(e, _) -> yield Expression(e)
                                | None -> ()
                        | SynExpr.ObjExpr(synType, expressionAndIdentifier, bindings, interfaces, _, _) -> 
                            yield Type(synType)
                            for x in bindings do yield Binding(x)
                        | SynExpr.For(_, _, expression, _, expression1, expression2, _) -> 
                            yield Expression(expression)
                            yield Expression(expression1)
                            yield Expression(expression2)
                        | SynExpr.ForEach(_, _, _, pattern, expression, expression1, _) -> 
                            yield Pattern(pattern)
                            yield Expression(expression)
                            yield Expression(expression1)
                        | SynExpr.Do(expression, _)
                        | SynExpr.Assert(expression, _)
                        | SynExpr.CompExpr(_, _, expression, _)
                        | SynExpr.ArrayOrListOfSeqExpr(_, expression, _) -> 
                            yield Expression(expression)
                        | SynExpr.Lambda(_, _, simplePatterns, expression, _) -> 
                            yield SimplePatterns(simplePatterns)
                            yield Expression(expression)
                        | SynExpr.MatchLambda(_, _, matchClauses, _, _) -> 
                            for x in matchClauses do yield Match(x)
                        | SynExpr.Match(_, expression, matchClauses, _, _) -> 
                            yield Expression(expression)
                            for x in matchClauses do yield Match(x)
                        | SynExpr.TypeApp(expression, _, types, _, _, _, _) -> 
                            yield Expression(expression)
                            for x in types do yield Type(x)
                        | SynExpr.LetOrUse(_, _, bindings, expression, _) -> 
                            for x in bindings do yield Binding(x)
                            yield Expression(expression)
                        | SynExpr.TryWith(expression, _, matchClauses, _, _, _, _) -> 
                            yield Expression(expression)
                            for x in matchClauses do yield Match(x)
                        | SynExpr.TryFinally(expression, expression1, _, _, _) -> 
                            yield Expression(expression)
                            yield Expression(expression1)
                        | SynExpr.IfThenElse(expression, expression1, expression2, _, _, _, _) -> 
                            yield Expression(expression)
                            yield Expression(expression1)
                            match expression2 with
                                | Some(e) -> yield Expression(e)
                                | None -> ()
                        | SynExpr.DotGet(expression, _, _, _)
                        | SynExpr.LongIdentSet(_, expression, _) -> 
                            yield Expression(expression)
                        | SynExpr.DotSet(expression, _, expression1, _) -> 
                            yield Expression(expression)
                            yield Expression(expression1)
                        | SynExpr.DotIndexedGet(expression, _, _, _) -> 
                            yield Expression(expression)
                        | SynExpr.DotNamedIndexedPropertySet(expression, _, expression1, expression2, _) -> 
                            yield Expression(expression)
                            yield Expression(expression1)
                            yield Expression(expression2)
                        | SynExpr.New(_, synType, expression, _) 
                        | SynExpr.TypeTest(expression, synType, _)
                        | SynExpr.Upcast(expression, synType, _)
                        | SynExpr.Downcast(expression, synType, _) -> 
                            yield Expression(expression)
                            yield Type(synType)
                        | SynExpr.AddressOf(_, expression, _, _) 
                        | SynExpr.InferredDowncast(expression, _)
                        | SynExpr.InferredUpcast(expression, _)
                        | SynExpr.DoBang(expression, _)
                        | SynExpr.Lazy(expression, _)
                        | SynExpr.TraitCall(_, _, expression, _) -> 
                            yield Expression(expression)
                        | SynExpr.ImplicitZero(_) -> ()
                        | SynExpr.YieldOrReturn(_, expression, _)
                        | SynExpr.YieldOrReturnFrom(_, expression, _) -> 
                            yield Expression(expression)
                        | SynExpr.LetOrUseBang(_, _, _, pattern, expression, expression1, _) -> 
                            yield Pattern(pattern)
                            yield Expression(expression)
                            yield Expression(expression1)
                        | SynExpr.Ident(_) 
                        | SynExpr.LongIdent(_, _, _, _) 
                        | SynExpr.Null(_)
                        | SynExpr.Const(_, _)
                        | SynExpr.DiscardAfterMissingQualificationAfterDot(_, _)
                        | SynExpr.FromParseError(_, _)
                        | SynExpr.LibraryOnlyILAssembly(_, _, _, _, _)
                        | SynExpr.LibraryOnlyStaticOptimization(_, _, _, _)
                        | SynExpr.LibraryOnlyUnionCaseFieldGet(_, _, _, _)
                        | SynExpr.LibraryOnlyUnionCaseFieldSet(_, _, _, _, _)
                        | SynExpr.ArbitraryAfterError(_, _) -> ()
                | SimplePattern(simplePattern) ->
                    match simplePattern with
                        | SynSimplePat.Id(identifier, _, isCompilerGenerated, _, _, range) -> ()
                        | SynSimplePat.Typed(simplePattern, synType, _) -> 
                            yield SimplePattern(simplePattern)
                            yield Type(synType)
                        | SynSimplePat.Attrib(simplePattern, attributes, _) -> 
                            yield SimplePattern(simplePattern)
                | SimplePatterns(simplePatterns) ->
                    match simplePatterns with
                        | SynSimplePats.SimplePats(simplePatterns, _) -> 
                            for x in simplePatterns do yield SimplePattern(x)
                        | SynSimplePats.Typed(simplePatterns, synType, _) -> 
                            yield SimplePatterns(simplePatterns)
                            yield Type(synType)
                | Pattern(pattern) ->
                    match pattern with
                        | SynPat.Named(pattern, _, _, _, _) -> 
                            yield Pattern(pattern)
                        | SynPat.Typed(pattern, synType, _) -> 
                            yield Pattern(pattern)
                            yield Type(synType)
                        | SynPat.Or(pattern, pattern1, _) -> 
                            yield Pattern(pattern)
                            yield Pattern(pattern1)
                        | SynPat.ArrayOrList(_, patterns, _)
                        | SynPat.Ands(patterns, _) -> 
                            for x in patterns do yield Pattern(x)
                        | SynPat.LongIdent(_, _, _, constructorArguments, _, _) -> 
                            yield ConstructorArguments(constructorArguments)
                        | SynPat.Tuple(patterns, _) -> 
                            for x in patterns do yield Pattern(x)
                        | SynPat.Attrib(pattern, _, _)
                        | SynPat.Paren(pattern, _) -> 
                            yield Pattern(pattern)
                        | SynPat.Record(patternsAndIdentifier, _) -> 
                            for (_, x) in patternsAndIdentifier do 
                                yield Pattern(x)
                        | SynPat.Null(_) -> ()
                        | SynPat.OptionalVal(identifier, _) -> ()
                        | SynPat.IsInst(synType, _) -> 
                            yield Type(synType)
                        | SynPat.QuoteExpr(expression, _) -> 
                            yield Expression(expression)
                        | SynPat.Const(_, _)
                        | SynPat.Wild(_)
                        | SynPat.FromParseError(_)
                        | SynPat.InstanceMember(_, _, _, _, _)
                        | SynPat.DeprecatedCharRange(_, _, _) -> ()
                | ConstructorArguments(arguments) ->
                    match arguments with
                        | SynConstructorArgs.Pats(patterns) -> 
                            for x in patterns do yield Pattern(x)
                        | SynConstructorArgs.NamePatPairs(namePatterns, _) -> 
                            for (_, x) in namePatterns do yield Pattern(x)
                | InterfaceImplementation(implementation) ->
                    match implementation with
                        | SynInterfaceImpl.InterfaceImpl(synType, bindings, _) -> 
                            yield Type(synType)
                            for x in bindings do yield Binding(x)
                | TypeParameter(typeParameter) ->
                    match typeParameter with 
                        | SynTypar.Typar(_, _, _) -> ()
        ]

    type CurrentNode =
        {
            Node: AstNode
            ChildNodes: AstNode list
            Breadcrumbs: AstNode list
        }

    /// Defines a function that visits a node on the AST.
    type Visitor = CurrentNode -> VisitorResult
    and 
        /// Defines a function that a visitor will return when it wants to supply 
        /// specific visitors for the node its visiting's children
        GetVisitorForChild = int -> AstNode -> Visitor option
    and 
        /// The return value of a visitor that lets the it specify how the 
        /// children of a visited node should be visited.
        VisitorResult =
            | Continue
            | Stop
            | ContinueWithVisitor of Visitor
            | ContinueWithVisitorsForChildren of GetVisitorForChild

    /// Visits a node with a single visitor, and returns each visitor to be used for each child node.
    /// This allows a visitor to decide how the children of the visited node shall be visited.
    let visit node visitor =
         match visitor node with
            | Continue -> 
                node.ChildNodes |> List.map (fun _ -> Some(visitor))
            | Stop -> 
                node.ChildNodes |> List.map (fun _ -> None)
            | ContinueWithVisitor(visitor) -> 
                node.ChildNodes |> List.map (fun _ -> Some(visitor))
            | ContinueWithVisitorsForChildren(getVisitorForChild) -> 
                node.ChildNodes |> List.mapi (fun i child -> getVisitorForChild i child)
        
    /// <param name="finishEarly">States whether to stop walking the tree, used for asynchronous environments to cancel the task.</param>
    /// <param name="breadcrumbs">List of parent nodes e.g. (parent, parent of parent, ...).</param>
    let rec walk finishEarly breadcrumbs visitors node = 
        let walk = walk finishEarly (node :: breadcrumbs)

        let children = traverseNode node

        let currentNode = { Node = node; ChildNodes = children; Breadcrumbs = breadcrumbs }

        let visitorsForChildren = visitors |> List.map (visit currentNode)

        if not <| finishEarly() then
            children |> List.iteri (fun i child ->
                let visitors = 
                    visitorsForChildren 
                        |> List.map (fun v -> v.[i])
                        |> List.choose id

                walk visitors child)

    let walkFile finishEarly visitors = function
        | ParsedInput.ImplFile(ParsedImplFileInput(_,_,_,_,_,moduleOrNamespaces,_))-> 
            moduleOrNamespaces |> List.iter (fun x -> walk finishEarly [] visitors (ModuleOrNamespace(x)))
        | ParsedInput.SigFile _ -> ()

    exception ParseException of string

    /// Parse a file.
    let parse finishEarly (checker:InteractiveChecker) projectOptions file input visitors =
        let parseFileResults = checker.ParseFileInProject(file, input, projectOptions) |> Async.RunSynchronously
        match parseFileResults.ParseTree with
        | Some tree -> 
            let checkFileResults = 
                checker.CheckFileInProject(parseFileResults, file, 0, input, projectOptions) 
                |> Async.RunSynchronously

            match checkFileResults with
            | CheckFileAnswer.Succeeded(res) -> 
                let visitors = visitors |> List.map (fun visitor -> visitor res)
                walkFile finishEarly visitors tree
            | res -> raise <| ParseException(sprintf "Parsing did not finish... (%A)" res)
        | None -> 
            let error = sprintf "Failed to parse file %s, probably missing FSharp.Core .sigdata and .opdata files." file
            raise <| ParseException(error) 

    /// Parse a single string.
    let parseInput input visitors =
        let checker = InteractiveChecker.Create()

        let file = "/home/user/Dog.test.fsx"
        
        let projectOptions = checker.GetProjectOptionsFromScript(file, input) |> Async.RunSynchronously

        parse (fun _ -> false) checker projectOptions file input visitors |> ignore