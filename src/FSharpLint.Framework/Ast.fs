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

/// Used to walk the FSharp Compiler's abstract syntax tree,
/// so that each node can be visited by a list of visitors.
module Ast =

    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices

    /// Represents a SuppressedMessageAttribute found in the AST.
    type SuppressedMessage =
        {
            /// Category property of the SuppressedMessageAttribute. (The name of the analyser to be suppressed).
            Category: string

            /// CheckId property of the SuppressedMessageAttribute. (The name of the rule to be suppressed).
            Rule: string
        }
    
    /// Passed to each visitor to provide them with access to the configuration and a way of reporting errors.
    type VisitorInfo =
        {
            /// Version of F# the source that's being analysed was written in.
            FSharpVersion: Version

            /// The current lint config to be used by visitors.
            Config: Configuration.Configuration

            /// Used by visitors to report errors.
            PostError: range -> string -> unit
        }

        member this.UseTypeChecker
            with get() = 
                match this.Config.UseTypeChecker with
                    | Some(true) -> true
                    | Some(_) | None -> false

    /// Nodes in the AST to be visited.
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

    /// Gets any SuppressMessageAttributes that are applied to a given node in the AST.
    let getSuppressMessageAttributes node =
        let tryGetArguments (attribute:SynAttribute) = 
            let tryGetArgumentsFromPropertyInitialisers arguments = 
                let rec getPropertyIntiailiserValues category checkid = function
                    | SynExpr.App(_, 
                                  _, 
                                  SynExpr.App(_, _, SynExpr.Ident(op), SynExpr.Ident(propName), _), 
                                  SynExpr.Const(SynConst.String(argValue, _), _), _)::tail when op.idText = "op_Equality" -> 
                        if propName.idText = "Category" then
                            getPropertyIntiailiserValues (Some(argValue)) checkid tail
                        else if propName.idText = "CheckId" then
                            getPropertyIntiailiserValues category (Some(argValue)) tail
                        else
                            getPropertyIntiailiserValues category checkid tail
                    | _::tail ->
                        getPropertyIntiailiserValues category checkid tail
                    | [] -> 
                        match category, checkid with
                            | Some(category), Some(checkid) ->
                                Some({ Category = category; Rule = checkid })
                            | _ -> None

                getPropertyIntiailiserValues None None arguments

            match attribute.ArgExpr with
                | SynExpr.Paren(SynExpr.Tuple(arguments, _, _), _, _, _) ->
                    match arguments with
                        | SynExpr.Const(SynConst.String(category, _), _)::SynExpr.Const(SynConst.String(checkid, _), _)::_ ->
                            Some({ Category = category; Rule = checkid })
                        | _ -> 
                            tryGetArgumentsFromPropertyInitialisers arguments
                | _ -> None

        let tryGetSuppressMessageAttribute (attribute:SynAttribute) =
            let attributeName =
                attribute.TypeName.Lid
                    |> List.rev
                    |> List.head

            if attributeName.idText = "SuppressMessage" || attributeName.idText = "SuppressMessageAttribute" then
                tryGetArguments attribute
            else 
                None

        match node with
            | ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_, _, _, _, attributes, _, range))
            | Binding(SynBinding.Binding(_, _, _, _, attributes, _, _, _, _, _, range, _))
            | ExceptionDefinition(SynExceptionDefn.ExceptionDefn(SynExceptionRepr.ExceptionDefnRepr(attributes, _, _, _, _, _), _, range))
            | ModuleDeclaration(SynModuleDecl.NestedModule(SynComponentInfo.ComponentInfo(attributes, _, _, _, _, _, _, _), _, _, range))
            | TypeDefinition(SynTypeDefn.TypeDefn(SynComponentInfo.ComponentInfo(attributes, _, _, _, _, _, _, _), _, _, range)) -> 
                attributes
                    |> List.choose tryGetSuppressMessageAttribute
                    |> List.map (fun x -> (x, range))
            | _ -> []

    /// Extracts the child nodes to be visited from a given node.
    [<System.Diagnostics.CodeAnalysis.SuppressMessage("SourceLength", "MaxLinesInFunction")>]
    [<System.Diagnostics.CodeAnalysis.SuppressMessage("CyclomaticComplexity", "*")>]
    let traverseNode node =
        [
            match node with
                | ModuleDeclaration(SynModuleDecl.NestedModule(componentInfo, moduleDeclarations, _, _)) ->
                    yield ComponentInfo(componentInfo)
                    for x in moduleDeclarations do yield ModuleDeclaration(x)
                | ModuleDeclaration(SynModuleDecl.Let(_, bindings, _)) ->
                    for x in bindings do yield Binding(x)
                | ModuleDeclaration(SynModuleDecl.DoExpr(_, expression, _)) ->
                    yield Expression(expression)
                | ModuleDeclaration(SynModuleDecl.Types(typeDefinitions, _)) ->
                    for x in typeDefinitions do yield TypeDefinition(x)
                | ModuleDeclaration(SynModuleDecl.Exception(exceptionDefinition, _)) ->
                    yield ExceptionDefinition(exceptionDefinition)
                | ModuleDeclaration(SynModuleDecl.NamespaceFragment(moduleOrNamespace)) ->
                    yield ModuleOrNamespace(moduleOrNamespace)
                | ModuleDeclaration(SynModuleDecl.Open(_))
                | ModuleDeclaration(SynModuleDecl.Attributes(_))
                | ModuleDeclaration(SynModuleDecl.HashDirective(_))
                | ModuleDeclaration(SynModuleDecl.ModuleAbbrev(_)) -> ()
                | ModuleOrNamespace(SynModuleOrNamespace(_, _, moduleDeclarations, _, _, _, _)) ->
                    for x in moduleDeclarations do yield ModuleDeclaration(x)
                | Binding(SynBinding.Binding(_, _, _, _, _, _, _, pattern, _, expression, _, _)) ->
                    yield Pattern(pattern)
                    yield Expression(expression)
                | ExceptionDefinition(SynExceptionDefn.ExceptionDefn(exceptionRepresentation, members, _)) ->
                    yield ExceptionRepresentation(exceptionRepresentation)
                    for x in members do yield MemberDefinition(x)
                | ExceptionRepresentation(SynExceptionRepr.ExceptionDefnRepr(_, unionCase, _, _, _, _)) ->
                    yield UnionCase(unionCase)
                | TypeDefinition(SynTypeDefn.TypeDefn(componentInfo, typeRepresentation, members, _)) ->
                    yield ComponentInfo(componentInfo)
                    yield TypeRepresentation(typeRepresentation)
                    for x in members do yield MemberDefinition(x)
                | TypeRepresentation(ObjectModel(_, members, _)) ->
                    for x in members do yield MemberDefinition(x)
                | TypeRepresentation(Simple(typeSimpleRepresentation, _)) ->
                    yield TypeSimpleRepresentation(typeSimpleRepresentation)
                | TypeSimpleRepresentation(SynTypeDefnSimpleRepr.Union(_, unionCases, _)) ->
                    for x in unionCases do yield UnionCase(x)
                | TypeSimpleRepresentation(SynTypeDefnSimpleRepr.Enum(enumCases, _)) ->
                    for x in enumCases do yield EnumCase(x)
                | TypeSimpleRepresentation(SynTypeDefnSimpleRepr.Record(_, fields, _)) ->
                    for x in fields do yield Field(x)
                | TypeSimpleRepresentation(SynTypeDefnSimpleRepr.TypeAbbrev(_, synType, _)) ->
                    yield Type(synType)
                | TypeSimpleRepresentation(SynTypeDefnSimpleRepr.General(_))
                | TypeSimpleRepresentation(SynTypeDefnSimpleRepr.LibraryOnlyILAssembly(_))
                | TypeSimpleRepresentation(SynTypeDefnSimpleRepr.None(_)) -> ()
                | Field(SynField.Field(_, _, _, synType, _, _, _, _)) ->
                    yield Type(synType)
                | Type(SynType.App(synType, _, types, _, _, _, _)) 
                | Type(SynType.LongIdentApp(synType, _, _, types, _, _, _)) ->
                    yield Type(synType)
                    for x in types do yield Type(x)
                | Type(SynType.Tuple(types, _)) ->
                    for (_, x) in types do yield Type(x)
                | Type(SynType.Fun(synType, synType1, _))
                | Type(SynType.StaticConstantNamed(synType, synType1, _))
                | Type(SynType.MeasureDivide(synType, synType1, _)) ->
                    yield Type(synType)
                    yield Type(synType1)
                | Type(SynType.WithGlobalConstraints(synType, _, _))
                | Type(SynType.HashConstraint(synType, _))
                | Type(SynType.MeasurePower(synType, _, _))
                | Type(SynType.Array(_, synType, _)) ->
                    yield Type(synType)
                | Type(SynType.StaticConstantExpr(expression, _)) ->
                    yield Expression(expression)
                | Type(SynType.Var(_))
                | Type(SynType.Anon(_))
                | Type(SynType.StaticConstant(_))
                | Type(SynType.LongIdent(_)) -> ()
                | Match(SynMatchClause.Clause(pattern, expression, expression1, _, _)) ->
                    yield Pattern(pattern)
                    match expression with 
                        | Some(x) -> yield Expression(x)
                        | None -> ()
                    yield Expression(expression1)
                | MemberDefinition(SynMemberDefn.Member(binding, _)) ->
                    yield Binding(binding)
                | MemberDefinition(SynMemberDefn.ImplicitCtor(_, _, patterns, _, _)) ->
                    for x in patterns do yield SimplePattern(x)
                | MemberDefinition(SynMemberDefn.ImplicitInherit(synType, expression, _, _)) ->
                    yield Type(synType)
                    yield Expression(expression)
                | MemberDefinition(SynMemberDefn.LetBindings(bindings, _, _, _)) ->
                    for x in bindings do yield Binding(x)
                | MemberDefinition(SynMemberDefn.Interface(synType, members, _)) ->
                    yield Type(synType)
                    match members with
                        | Some(members) -> for x in members do yield MemberDefinition(x)
                        | None -> ()
                | MemberDefinition(SynMemberDefn.Inherit(synType, _, _)) ->
                    yield Type(synType)
                | MemberDefinition(SynMemberDefn.ValField(field, _)) ->
                    yield Field(field)
                | MemberDefinition(SynMemberDefn.NestedType(typeDefinition, _, _)) ->
                    yield TypeDefinition(typeDefinition)
                | MemberDefinition(SynMemberDefn.AutoProperty(_, _, _, synType, _, _, _, _, expression, _, _)) ->
                    match synType with
                        | Some(synType) -> yield Type(synType)
                        | None -> ()
                    yield Expression(expression)
                | MemberDefinition(SynMemberDefn.Open(_))
                | MemberDefinition(SynMemberDefn.AbstractSlot(_)) -> ()
                | Expression(SynExpr.Paren(expression, _, _, _))
                | Expression(SynExpr.DotGet(expression, _, _, _))
                | Expression(SynExpr.DotIndexedGet(expression, _, _, _))
                | Expression(SynExpr.LongIdentSet(_, expression, _))
                | Expression(SynExpr.Do(expression, _))
                | Expression(SynExpr.Assert(expression, _))
                | Expression(SynExpr.CompExpr(_, _, expression, _))
                | Expression(SynExpr.ArrayOrListOfSeqExpr(_, expression, _))
                | Expression(SynExpr.AddressOf(_, expression, _, _))
                | Expression(SynExpr.InferredDowncast(expression, _))
                | Expression(SynExpr.InferredUpcast(expression, _))
                | Expression(SynExpr.DoBang(expression, _))
                | Expression(SynExpr.Lazy(expression, _))
                | Expression(SynExpr.TraitCall(_, _, expression, _))
                | Expression(SynExpr.YieldOrReturn(_, expression, _))
                | Expression(SynExpr.YieldOrReturnFrom(_, expression, _)) ->
                    yield Expression(expression)
                | Expression(SynExpr.Quote(expression, _, expression1, _, _))
                | Expression(SynExpr.App(_, _, expression, expression1, _))
                | Expression(SynExpr.Sequential(_, _, expression, expression1, _))
                | Expression(SynExpr.NamedIndexedPropertySet(_, expression, expression1, _))
                | Expression(SynExpr.DotIndexedSet(expression, _, expression1, _, _, _))
                | Expression(SynExpr.JoinIn(expression, _, expression1, _))
                | Expression(SynExpr.While(_, expression, expression1, _))
                | Expression(SynExpr.TryFinally(expression, expression1, _, _, _))
                | Expression(SynExpr.DotSet(expression, _, expression1, _)) ->
                    yield Expression(expression)
                    yield Expression(expression1)
                | Expression(SynExpr.Typed(expression, synType, _)) ->
                    yield Expression(expression)
                    yield Type(synType)
                | Expression(SynExpr.Tuple(expressions, _, _))
                | Expression(SynExpr.ArrayOrList(_, expressions, _)) ->
                    for x in expressions do yield Expression(x)
                | Expression(SynExpr.Record(_, expression, _, _)) ->
                    match expression with
                        | Some(e, _) -> yield Expression(e)
                        | None -> ()
                | Expression(SynExpr.ObjExpr(synType, _, bindings, _, _, _)) ->
                    yield Type(synType)
                    for x in bindings do yield Binding(x)
                | Expression(SynExpr.DotNamedIndexedPropertySet(expression, _, expression1, expression2, _))
                | Expression(SynExpr.For(_, _, expression, _, expression1, expression2, _)) ->
                    yield Expression(expression)
                    yield Expression(expression1)
                    yield Expression(expression2)
                | Expression(SynExpr.LetOrUseBang(_, _, _, pattern, expression, expression1, _))
                | Expression(SynExpr.ForEach(_, _, _, pattern, expression, expression1, _)) ->
                    yield Pattern(pattern)
                    yield Expression(expression)
                    yield Expression(expression1)
                | Expression(SynExpr.Lambda(_, _, simplePatterns, expression, _)) ->
                    yield SimplePatterns(simplePatterns)
                    yield Expression(expression)
                | Expression(SynExpr.MatchLambda(_, _, matchClauses, _, _)) ->
                    for x in matchClauses do yield Match(x)
                | Expression(SynExpr.Match(_, expression, matchClauses, _, _)) ->
                    yield Expression(expression)
                    for x in matchClauses do yield Match(x)
                | Expression(SynExpr.TypeApp(expression, _, types, _, _, _, _)) ->
                    yield Expression(expression)
                    for x in types do yield Type(x)
                | Expression(SynExpr.LetOrUse(_, _, bindings, expression, _)) ->
                    for x in bindings do yield Binding(x)
                    yield Expression(expression)
                | Expression(SynExpr.TryWith(expression, _, matchClauses, _, _, _, _)) ->
                    yield Expression(expression)
                    for x in matchClauses do yield Match(x)
                | Expression(SynExpr.IfThenElse(expression, expression1, expression2, _, _, _, _)) ->
                    yield Expression(expression)
                    yield Expression(expression1)
                    match expression2 with
                        | Some(e) -> yield Expression(e)
                        | None -> ()
                | Expression(SynExpr.New(_, synType, expression, _) )
                | Expression(SynExpr.TypeTest(expression, synType, _))
                | Expression(SynExpr.Upcast(expression, synType, _))
                | Expression(SynExpr.Downcast(expression, synType, _)) ->
                    yield Expression(expression)
                    yield Type(synType)
                | Expression(SynExpr.ImplicitZero(_))
                | Expression(SynExpr.Ident(_))
                | Expression(SynExpr.LongIdent(_))
                | Expression(SynExpr.Null(_))
                | Expression(SynExpr.Const(_))
                | Expression(SynExpr.DiscardAfterMissingQualificationAfterDot(_))
                | Expression(SynExpr.FromParseError(_))
                | Expression(SynExpr.LibraryOnlyILAssembly(_))
                | Expression(SynExpr.LibraryOnlyStaticOptimization(_))
                | Expression(SynExpr.LibraryOnlyUnionCaseFieldGet(_))
                | Expression(SynExpr.LibraryOnlyUnionCaseFieldSet(_))
                | Expression(SynExpr.ArbitraryAfterError(_)) -> 
                    ()
                | SimplePattern(SynSimplePat.Id(_)) -> ()
                | SimplePattern(SynSimplePat.Typed(simplePattern, synType, _)) ->
                    yield SimplePattern(simplePattern)
                    yield Type(synType)
                | SimplePattern(SynSimplePat.Attrib(simplePattern, _, _)) ->
                    yield SimplePattern(simplePattern)
                | SimplePatterns(SynSimplePats.SimplePats(simplePatterns, _)) ->
                    for x in simplePatterns do yield SimplePattern(x)
                | SimplePatterns(SynSimplePats.Typed(simplePatterns, synType, _)) ->
                    yield SimplePatterns(simplePatterns)
                    yield Type(synType)
                | Pattern(SynPat.Named(pattern, _, _, _, _)) ->
                    yield Pattern(pattern)
                | Pattern(SynPat.Typed(pattern, synType, _)) ->
                    yield Pattern(pattern)
                    yield Type(synType)
                | Pattern(SynPat.Or(pattern, pattern1, _)) ->
                    yield Pattern(pattern)
                    yield Pattern(pattern1)
                | Pattern(SynPat.ArrayOrList(_, patterns, _))
                | Pattern(SynPat.Ands(patterns, _)) ->
                    for x in patterns do yield Pattern(x)
                | Pattern(SynPat.LongIdent(_, _, _, constructorArguments, _, _)) ->
                    yield ConstructorArguments(constructorArguments)
                | Pattern(SynPat.Tuple(patterns, _)) ->
                    for x in patterns do yield Pattern(x)
                | Pattern(SynPat.Attrib(pattern, _, _))
                | Pattern(SynPat.Paren(pattern, _)) ->
                    yield Pattern(pattern)
                | Pattern(SynPat.Record(patternsAndIdentifier, _)) ->
                    for (_, x) in patternsAndIdentifier do 
                        yield Pattern(x)
                | Pattern(SynPat.IsInst(synType, _)) ->
                    yield Type(synType)
                | Pattern(SynPat.QuoteExpr(expression, _)) ->
                    yield Expression(expression)
                | Pattern(SynPat.Const(_))
                | Pattern(SynPat.Wild(_))
                | Pattern(SynPat.FromParseError(_))
                | Pattern(SynPat.InstanceMember(_))
                | Pattern(SynPat.DeprecatedCharRange(_))
                | Pattern(SynPat.Null(_))
                | Pattern(SynPat.OptionalVal(_)) -> ()
                | ConstructorArguments(SynConstructorArgs.Pats(patterns)) ->
                    for x in patterns do yield Pattern(x)
                | ConstructorArguments(SynConstructorArgs.NamePatPairs(namePatterns, _)) ->
                    for (_, x) in namePatterns do yield Pattern(x)
                | InterfaceImplementation(SynInterfaceImpl.InterfaceImpl(synType, bindings, _)) ->
                    yield Type(synType)
                    for x in bindings do yield Binding(x)
                | TypeParameter(_)
                | UnionCase(_)
                | ComponentInfo(_)
                | EnumCase(_) -> ()
        ]

    /// Contains information on the current node being visited.
    type CurrentNode =
        {
            Node: AstNode
            ChildNodes: AstNode list

            /// A list of parent nodes e.g. parent, grand parent, grand grand parent.
            Breadcrumbs: AstNode list

            /// Suppressed message attributes that have been applied to the block of code 
            /// the current node is within.
            SuppressedMessages: (SuppressedMessage * range) list
        }

        with
            /// Has a given rule been suppressed by SuppressMessageAttribute?
            member this.IsSuppressed(analyserName, ?rulename) =
                let isAnalyserSuppressed (analyser, _) =
                    analyser.Category = analyserName && 
                    (Option.exists ((=) analyser.Rule) rulename || analyser.Rule = "*")

                this.SuppressedMessages |> List.exists isAnalyserSuppressed

    /// Defines a function that visits a node on the AST.
    type Visitor = CurrentNode -> VisitorResult
    and 
        /// Defines a function that a visitor will return when it wants to supply 
        /// specific visitors for the node its visiting's children
        GetVisitorForChild = int -> AstNode -> Visitor option
    and 
        /// The return value of a visitor that lets the it specify how other nodes should be visited.
        /// Using partial application you can apply state to each visitor returned, 
        /// allowing for things such as summing the number of if statements in a function to be done purely.
        VisitorResult =
            /// Visit children with the current visitor.
            | Continue
            /// Do not visit any children.
            | Stop
            /// Enables state to be passed down to children.
            | ContinueWithVisitor of Visitor
            /// Enables state to be passed down to certain children.
            | ContinueWithVisitorsForChildren of GetVisitorForChild
            /// Enables state to be passed along a walk of a tree. 
            /// e.g. to sum the number of if statements in a function.
            | WalkWithVisitor of Visitor * (unit -> unit)

    let private walkTreeToGetSuppressMessageAttributes rootNode =
        let rec walk node suppressedMessageAttributes =
            let getAttributesForChild attrs child = 
                getSuppressMessageAttributes child @ walk child attrs

            traverseNode node
                |> List.fold getAttributesForChild suppressedMessageAttributes

        walk rootNode (getSuppressMessageAttributes rootNode)

    /// Gets all the attributes used to suppress lint warnings in a file's AST for sections of code.
    /// This function is for plaintext visitors so they can get the suppressions.
    let getSuppressMessageAttributesFromAst = function
        | ParsedInput.ImplFile(ParsedImplFileInput(_,_,_,_,_,roots,_)) ->
            let walkRoot root =
                ModuleOrNamespace(root)
                    |> walkTreeToGetSuppressMessageAttributes

            roots |> List.collect walkRoot
        | ParsedInput.SigFile(_) -> []

    type ToWalk =
    | Node of AstNode * AstNode list * (SuppressedMessage * range) list * Visitor
    | EndOfWalk
        
    /// <summary>
    /// Walks an abstract syntax tree from a given root node and applies a visitor to each node in the tree.
    /// Maintains state of visitors using the visitor's return value.
    /// </summary>
    /// <param name="finishEarly">
    /// States whether to stop walking the tree, used for asynchronous environments to cancel the task.
    /// </param>
    let private walk finishEarly rootNode visitor =
        let nodesToVisit = Stack<_>()

        nodesToVisit.Push(Node(rootNode, [], [], visitor))

        let mutable walkVisitor = None

        while (Seq.isEmpty >> not) nodesToVisit && (finishEarly >> not) () do
            match nodesToVisit.Pop() with
            | Node(node, breadcrumbs, suppressedMessages, visitor) -> 
                let suppressedMessages = getSuppressMessageAttributes node @ suppressedMessages
                        
                let children = traverseNode node |> List.rev

                let currentNode = 
                    { 
                        Node = node
                        ChildNodes = children
                        Breadcrumbs = breadcrumbs
                        SuppressedMessages = suppressedMessages
                    }

                let breadcrumbs = node :: breadcrumbs

                let visitor =
                    match walkVisitor with
                    | Some(visitor, _) -> visitor
                    | None -> visitor
            
                match visitor currentNode with
                /// Visit children with the current visitor.
                | Continue -> 
                    for child in children do 
                        nodesToVisit.Push(Node(child, breadcrumbs, suppressedMessages, visitor))

                /// Enables state to be passed down to children.
                | ContinueWithVisitor(visitor) -> 
                    for child in children do
                        nodesToVisit.Push(Node(child, breadcrumbs, suppressedMessages, visitor))

                /// Enables state to be passed down to certain children.
                | ContinueWithVisitorsForChildren(getVisitor) -> 
                    let stackChildren i child =
                        let visitor =
                            match getVisitor i child with
                            | Some(visitor) -> visitor
                            | None -> visitor

                        nodesToVisit.Push(Node(child, breadcrumbs, suppressedMessages, visitor))

                    Seq.iteri (fun i -> stackChildren (Seq.length children - i - 1)) children

                /// Enables state to be passed along a walk of a tree. 
                /// e.g. to sum the number of if statements in a function.
                | WalkWithVisitor(visitor, notify) -> 
                    if Option.isNone walkVisitor then
                        nodesToVisit.Push(EndOfWalk)

                    walkVisitor <- Some(visitor, notify)

                    for child in children do
                        nodesToVisit.Push(Node(child, breadcrumbs, suppressedMessages, visitor))

                /// Do not visit any children.
                | Stop -> ()
            | EndOfWalk -> 
                match walkVisitor with
                | Some(_, notify) -> notify()
                | None -> ()

                walkVisitor <- None

    /// Information for a file to be linted that is given to the visitors for them to analyse.
    type FileParseInfo =
        {
            /// Contents of the file.
            PlainText: string

            /// File represented as an AST.
            Ast: ParsedInput

            /// Optional results of inferring the types on the AST (allows for a more accurate lint).
            TypeCheckResults: FSharpCheckFileResults option

            /// Path to the file.
            File: string
        }

    /// Lint a file.
    let lintFile finishEarly fileInfo visitors =
        let visitorsWithTypeCheck = visitors |> List.map (fun visitor -> visitor fileInfo.TypeCheckResults)

        match fileInfo.Ast with
            | ParsedInput.ImplFile(ParsedImplFileInput(_,_,_,_,_,moduleOrNamespaces,_))-> 
                for moduleOrNamespace in moduleOrNamespaces do
                    Async.Parallel 
                        [
                            for visitor in visitorsWithTypeCheck -> 
                                async { return walk finishEarly (ModuleOrNamespace(moduleOrNamespace)) visitor }
                        ] 
                        |> Async.RunSynchronously 
                        |> ignore
            | ParsedInput.SigFile _ -> ()