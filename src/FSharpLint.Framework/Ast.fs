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

    type VisitorResult =
        | ContinueWalk
        | Stop

    let rec walk path visitors node = 
        let walk = walk (node :: path)

        let visitors = visitors |> List.filter (fun visit -> 
            match visit path node with
                | ContinueWalk -> true
                | Stop -> false)

        let walk = walk visitors

        match node with
            | ModuleDeclaration(moduleDeclaration) ->
                match moduleDeclaration with
                    | SynModuleDecl.NestedModule(componentInfo, moduleDeclarations, _, _) ->
                        ComponentInfo(componentInfo) |> walk
                        moduleDeclarations |> List.iter (fun x -> ModuleDeclaration(x) |> walk)
                    | SynModuleDecl.Let(_, bindings, _) -> 
                        bindings |> List.iter (fun x -> Binding(x) |> walk)
                    | SynModuleDecl.DoExpr(_, expression, _) -> 
                        Expression(expression) |> walk
                    | SynModuleDecl.Types(typeDefinitions, _) -> 
                        typeDefinitions |> List.iter (fun x -> TypeDefinition(x) |> walk)
                    | SynModuleDecl.Exception(exceptionDefinition, _) -> 
                        ExceptionDefinition(exceptionDefinition) |> walk
                    | SynModuleDecl.NamespaceFragment(moduleOrNamespace) -> 
                        ModuleOrNamespace(moduleOrNamespace) |> walk
                    | SynModuleDecl.Open(_, _)
                    | SynModuleDecl.Attributes(_, _)
                    | SynModuleDecl.HashDirective(_, _)
                    | SynModuleDecl.ModuleAbbrev(_, _, _) -> ()
            | ModuleOrNamespace(moduleOrNamespace) ->
                match moduleOrNamespace with
                    | SynModuleOrNamespace(_, _, moduleDeclarations, _, _, _, _) ->
                        moduleDeclarations |> List.iter (fun x -> ModuleDeclaration(x) |> walk)
            | Binding(binding) ->
                match binding with
                    | SynBinding.Binding(_, _, _, _, _, _, _, pattern, _, expression, _, _) ->
                        Pattern(pattern) |> walk
                        Expression(expression) |> walk
            | ExceptionDefinition(exceptionDefinition) ->
                match exceptionDefinition with
                    | SynExceptionDefn.ExceptionDefn(exceptionRepresentation, members, _) -> 
                        ExceptionRepresentation(exceptionRepresentation) |> walk
                        members |> List.iter (fun x -> MemberDefinition(x) |> walk)
            | ExceptionRepresentation(exceptionRepresentation) ->
                match exceptionRepresentation with
                    | SynExceptionRepr.ExceptionDefnRepr(_, unionCase, _, _, _, _) -> 
                        UnionCase(unionCase) |> walk
            | TypeDefinition(typeDefinition) ->
                match typeDefinition with
                    | SynTypeDefn.TypeDefn(componentInfo, typeRepresentation, members, _) -> 
                        ComponentInfo(componentInfo) |> walk
                        TypeRepresentation(typeRepresentation) |> walk
                        members |> List.iter (fun x -> MemberDefinition(x) |> walk)
            | ComponentInfo(componentInfo) ->
                match componentInfo with
                    | SynComponentInfo.ComponentInfo(_, _, _, _, _, _, _, _) -> ()
            | TypeRepresentation(typeRepresentation) ->
                match typeRepresentation with
                    | ObjectModel(typeKind, members, _) -> 
                        members |> List.iter (fun x -> MemberDefinition(x) |> walk)
                    | Simple(typeSimpleRepresentation, _) -> 
                        TypeSimpleRepresentation(typeSimpleRepresentation) |> walk
            | TypeSimpleRepresentation(typeSimpleRepresentation) ->
                match typeSimpleRepresentation with
                    | SynTypeDefnSimpleRepr.Union(access, unionCases, range) -> 
                        unionCases |> List.iter (fun x -> UnionCase(x) |> walk)
                    | SynTypeDefnSimpleRepr.Enum(enumCases, _) -> 
                        enumCases |> List.iter (fun x -> EnumCase(x) |> walk)
                    | SynTypeDefnSimpleRepr.Record(_, fields, _) -> 
                        fields |> List.iter (fun x -> Field(x) |> walk)
                    | SynTypeDefnSimpleRepr.TypeAbbrev(_, synType, _) -> 
                        Type(synType) |> walk
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
                        Type(synType) |> walk
            | Type(synType) ->
                match synType with
                    | SynType.App(synType, _, types, _, _, _, _) 
                    | SynType.LongIdentApp(synType, _, _, types, _, _, _) -> 
                        Type(synType) |> walk
                        types |> List.iter (fun x -> Type(x) |> walk)
                    | SynType.Tuple(types, _) ->    
                        types |> List.iter (fun (_, x) -> Type(x) |> walk)
                    | SynType.Fun(synType, synType1, _)
                    | SynType.StaticConstantNamed(synType, synType1, _)
                    | SynType.MeasureDivide(synType, synType1, _) -> 
                        Type(synType) |> walk
                        Type(synType1) |> walk
                    | SynType.WithGlobalConstraints(synType, _, _)
                    | SynType.HashConstraint(synType, _)
                    | SynType.MeasurePower(synType, _, _)
                    | SynType.Array(_, synType, _) -> 
                        Type(synType) |> walk
                    | SynType.StaticConstantExpr(expression, _) -> 
                        Expression(expression) |> walk
                    | SynType.Var(_, _)
                    | SynType.Anon(_)
                    | SynType.StaticConstant(_, _) 
                    | SynType.LongIdent(_) -> ()
            | Match(matchClause) ->
                match matchClause with
                    | SynMatchClause.Clause(pattern, expression, expression1, _, _) -> 
                        Pattern(pattern) |> walk
                        expression |> Option.iter (fun x -> Expression(x) |> walk)
                        Expression(expression1) |> walk
            | MemberDefinition(synMember) ->
                match synMember with
                    | SynMemberDefn.Member(binding, _) -> 
                        Binding(binding) |> walk
                    | SynMemberDefn.ImplicitCtor(_, attributes, patterns, identifier, _) -> 
                        patterns |> List.iter (fun x -> SimplePattern(x) |> walk)
                    | SynMemberDefn.ImplicitInherit(synType, expression, _, _) -> 
                        Type(synType) |> walk
                        Expression(expression) |> walk
                    | SynMemberDefn.LetBindings(bindings, _, _, _) -> 
                        bindings |> List.iter (fun x -> Binding(x) |> walk)
                    | SynMemberDefn.Interface(synType, members, _) -> 
                        Type(synType) |> walk
                        members |> Option.iter (fun x -> x |> List.iter (fun x -> MemberDefinition(x) |> walk))
                    | SynMemberDefn.Inherit(synType, _, _) -> 
                        Type(synType) |> walk
                    | SynMemberDefn.ValField(field, _) -> 
                        Field(field) |> walk
                    | SynMemberDefn.NestedType(typeDefinition, _, _) -> 
                        TypeDefinition(typeDefinition) |> walk
                    | SynMemberDefn.AutoProperty(attributes, _, identifier, synType, _, _, _, _, expression, _, _) -> 
                        synType |> Option.iter (fun x -> Type(x) |> walk)
                        Expression(expression) |> walk
                    | SynMemberDefn.Open(_, _)
                    | SynMemberDefn.AbstractSlot(_, _, _) -> ()
            | Expression(expression) ->
                match expression with
                    | SynExpr.Paren(expression, _, _, _) -> 
                        Expression(expression) |> walk
                    | SynExpr.Quote(expression, _, expression1, _, _)
                    | SynExpr.App(_, _, expression, expression1, _)
                    | SynExpr.Sequential(_, _, expression, expression1, _)
                    | SynExpr.NamedIndexedPropertySet(_, expression, expression1, _)
                    | SynExpr.DotIndexedSet(expression, _, expression1, _, _, _)
                    | SynExpr.JoinIn(expression, _, expression1, _)
                    | SynExpr.While(_, expression, expression1, _) -> 
                        Expression(expression) |> walk
                        Expression(expression1) |> walk
                    | SynExpr.Typed(expression, synType, _) -> 
                        Expression(expression) |> walk
                        Type(synType) |> walk
                    | SynExpr.Tuple(expressions, _, _)
                    | SynExpr.ArrayOrList(_, expressions, _) ->
                        expressions |> List.iter (fun x -> Expression(x) |> walk) 
                    | SynExpr.Record(synType, expression, _, _) -> 
                        expression |> Option.iter (fun (x, _) -> Expression(x) |> walk)
                    | SynExpr.ObjExpr(synType, expressionAndIdentifier, bindings, interfaces, _, _) -> 
                        Type(synType) |> walk
                        bindings |> List.iter (fun x -> Binding(x) |> walk)
                    | SynExpr.For(_, _, expression, _, expression1, expression2, _) -> 
                        Expression(expression) |> walk
                        Expression(expression1) |> walk
                        Expression(expression2) |> walk
                    | SynExpr.ForEach(_, _, _, pattern, expression, expression1, _) -> 
                        Pattern(pattern) |> walk
                        Expression(expression) |> walk
                        Expression(expression1) |> walk
                    | SynExpr.Do(expression, _)
                    | SynExpr.Assert(expression, _)
                    | SynExpr.CompExpr(_, _, expression, _)
                    | SynExpr.ArrayOrListOfSeqExpr(_, expression, _) -> 
                        Expression(expression) |> walk
                    | SynExpr.Lambda(_, _, simplePatterns, expression, _) -> 
                        SimplePatterns(simplePatterns) |> walk
                        Expression(expression) |> walk
                    | SynExpr.MatchLambda(_, _, matchClauses, _, _) -> 
                        matchClauses |> List.iter (fun x -> Match(x) |> walk)
                    | SynExpr.Match(_, expression, matchClauses, _, _) -> 
                        Expression(expression) |> walk
                        matchClauses |> List.iter (fun x -> Match(x) |> walk)
                    | SynExpr.TypeApp(expression, _, types, _, _, _, _) -> 
                        Expression(expression) |> walk
                        types |> List.iter (fun x -> Type(x) |> walk)
                    | SynExpr.LetOrUse(_, _, bindings, expression, _) -> 
                        bindings |> List.iter (fun x -> Binding(x) |> walk)
                        Expression(expression) |> walk
                    | SynExpr.TryWith(expression, _, matchClauses, _, _, _, _) -> 
                        Expression(expression) |> walk
                        matchClauses |> List.iter (fun x -> Match(x) |> walk)
                    | SynExpr.TryFinally(expression, expression1, _, _, _) -> 
                        Expression(expression) |> walk
                        Expression(expression1) |> walk
                    | SynExpr.IfThenElse(expression, expression1, expression2, _, _, _, _) -> 
                        Expression(expression) |> walk
                        Expression(expression1) |> walk
                        expression2 |> Option.iter (fun x -> Expression(x) |> walk)
                    | SynExpr.DotGet(expression, _, _, _)
                    | SynExpr.LongIdentSet(_, expression, _) -> 
                        Expression(expression) |> walk
                    | SynExpr.DotSet(expression, _, expression1, _) -> 
                        Expression(expression) |> walk
                        Expression(expression1) |> walk
                    | SynExpr.DotIndexedGet(expression, _, _, _) -> 
                        Expression(expression) |> walk
                    | SynExpr.DotNamedIndexedPropertySet(expression, _, expression1, expression2, _) -> 
                        Expression(expression) |> walk
                        Expression(expression1) |> walk
                        Expression(expression2) |> walk
                    | SynExpr.New(_, synType, expression, _) 
                    | SynExpr.TypeTest(expression, synType, _)
                    | SynExpr.Upcast(expression, synType, _)
                    | SynExpr.Downcast(expression, synType, _) -> 
                        Expression(expression) |> walk
                        Type(synType) |> walk
                    | SynExpr.AddressOf(_, expression, _, _) 
                    | SynExpr.InferredDowncast(expression, _)
                    | SynExpr.InferredUpcast(expression, _)
                    | SynExpr.DoBang(expression, _)
                    | SynExpr.Lazy(expression, _)
                    | SynExpr.TraitCall(_, _, expression, _) -> 
                        Expression(expression) |> walk
                    | SynExpr.ImplicitZero(_) -> ()
                    | SynExpr.YieldOrReturn(_, expression, _)
                    | SynExpr.YieldOrReturnFrom(_, expression, _) -> 
                        Expression(expression) |> walk
                    | SynExpr.LetOrUseBang(_, _, _, pattern, expression, expression1, _) -> 
                        Pattern(pattern) |> walk
                        Expression(expression) |> walk
                        Expression(expression1) |> walk
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
                        SimplePattern(simplePattern) |> walk
                        Type(synType) |> walk
                    | SynSimplePat.Attrib(simplePattern, attributes, _) -> 
                        SimplePattern(simplePattern) |> walk
            | SimplePatterns(simplePatterns) ->
                match simplePatterns with
                    | SynSimplePats.SimplePats(simplePatterns, _) -> 
                        simplePatterns |> List.iter (fun x -> SimplePattern(x) |> walk)
                    | SynSimplePats.Typed(simplePatterns, synType, _) -> 
                        SimplePatterns(simplePatterns) |> walk
                        Type(synType) |> walk
            | Pattern(pattern) ->
                match pattern with
                    | SynPat.Named(pattern, _, _, _, _) -> 
                        Pattern(pattern) |> walk
                    | SynPat.Typed(pattern, synType, _) -> 
                        Pattern(pattern) |> walk
                        Type(synType) |> walk
                    | SynPat.Or(pattern, pattern1, _) -> 
                        Pattern(pattern) |> walk
                        Pattern(pattern1) |> walk
                    | SynPat.ArrayOrList(_, patterns, _)
                    | SynPat.Ands(patterns, _) -> 
                        patterns |> List.iter (fun x -> Pattern(x) |> walk)
                    | SynPat.LongIdent(_, _, _, constructorArguments, _, _) -> 
                        ConstructorArguments(constructorArguments) |> walk
                    | SynPat.Tuple(patterns, _) -> 
                        patterns |> List.iter (fun x -> Pattern(x) |> walk)
                    | SynPat.Attrib(pattern, _, _)
                    | SynPat.Paren(pattern, _) -> 
                        Pattern(pattern) |> walk
                    | SynPat.Record(patternsAndIdentifier, _) -> 
                        patternsAndIdentifier |> List.iter (fun (_, pattern) -> Pattern(pattern) |> walk)
                    | SynPat.Null(_) -> ()
                    | SynPat.OptionalVal(identifier, _) -> ()
                    | SynPat.IsInst(synType, _) -> 
                        Type(synType) |> walk
                    | SynPat.QuoteExpr(expression, _) -> 
                        Expression(expression) |> walk
                    | SynPat.Const(_, _)
                    | SynPat.Wild(_)
                    | SynPat.FromParseError(_)
                    | SynPat.InstanceMember(_, _, _, _, _)
                    | SynPat.DeprecatedCharRange(_, _, _) -> ()
            | ConstructorArguments(arguments) ->
                match arguments with
                    | SynConstructorArgs.Pats(patterns) -> 
                       patterns |> List.iter (fun x -> Pattern(x) |> walk)
                    | SynConstructorArgs.NamePatPairs(namePatterns, _) -> 
                        namePatterns |> List.iter (fun (_, pattern) -> Pattern(pattern) |> walk)
            | InterfaceImplementation(implementation) ->
                match implementation with
                    | SynInterfaceImpl.InterfaceImpl(synType, bindings, _) -> 
                        Type(synType) |> walk
                        bindings |> List.iter (fun x -> Binding(x) |> walk)
            | TypeParameter(typeParameter) ->
                match typeParameter with 
                    | SynTypar.Typar(_, _, _) -> ()

    let walkFile visitors = function
        | ParsedInput.ImplFile(ParsedImplFileInput(_,_,_,_,_,moduleOrNamespaces,_))-> 
            moduleOrNamespaces |> List.iter (fun x -> walk [] visitors (ModuleOrNamespace(x)))
        | ParsedInput.SigFile _ -> ()

    exception ParseException of string

    /// Parse a file.
    let parse (checker:InteractiveChecker) projectOptions file input visitors =
        let parseFileResults = checker.ParseFileInProject(file, input, projectOptions)
        match parseFileResults.ParseTree with
        | Some tree -> 
            let checkFileResults = 
                checker.CheckFileInProject(parseFileResults, file, 0, input, projectOptions) 
                |> Async.RunSynchronously

            match checkFileResults with
            | CheckFileAnswer.Succeeded(res) -> 
                let visitors = visitors |> List.map (fun visitor -> visitor res)
                walkFile visitors tree
            | res -> raise <| ParseException(sprintf "Parsing did not finish... (%A)" res)
        | None -> 
            let error = sprintf "Failed to parse file %s, probably missing FSharp.Core .sigdata and .opdata files." file
            raise <| ParseException(error) 

    /// Parse a single string.
    let parseInput input visitors =
        let checker = InteractiveChecker.Create()

        let file = "/home/user/Dog.test.fsx"
        
        let projectOptions = checker.GetProjectOptionsFromScript(file, input)

        parse checker projectOptions file input visitors |> ignore