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
        { /// Category property of the SuppressedMessageAttribute. (The name of the analyser to be suppressed).
          Category: string

          /// CheckId property of the SuppressedMessageAttribute. (The name of the rule to be suppressed).
          Rule: string }
    
    /// Passed to each visitor to provide them with access to the configuration and a way of reporting errors.
    [<NoEquality>]
    [<NoComparison>]
    type VisitorInfo =
        {  /// Version of F# the source that's being analysed was written in.
          FSharpVersion: Version

          /// The current lint config to be used by visitors.
          Config: Configuration.Configuration

          /// Used by visitors to report errors.
          PostError: range -> string -> unit
          
          Text: string }

        member this.UseTypeChecker = 
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
        | Identifier of string list

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
            else None

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

    /// Extracts an expression from parentheses e.g. ((x + 4)) -> x + 4
    let rec removeParens = function
        | SynExpr.Paren(x, _, _, _) -> removeParens x
        | x -> x
        
    /// Inlines pipe operators to give a flat function application expression
    /// e.g. `x |> List.map id` to `List.map id x`. 
    let (|FuncApp|_|) functionApplication =
        let rec flatten flattened exprToFlatten =
            match exprToFlatten with
            | SynExpr.App(_, _, x, y, _) -> 
                match removeParens x with
                | SynExpr.App(_, true, SynExpr.Ident(op), rhs, _) as app ->
                    let lhs = removeParens y

                    match op.idText with
                    | "op_PipeRight" | "op_PipeRight2" | "op_PipeRight3" -> 
                        flatten [removeParens rhs] lhs
                    | "op_PipeLeft" | "op_PipeLeft2" | "op_PipeLeft3" -> 
                        flatten (removeParens lhs::flattened) (removeParens rhs)
                    | _ -> flatten (removeParens lhs::flattened) app
                | x -> 
                    let leftExpr, rightExpr = (x, removeParens y)
                    flatten (removeParens rightExpr::flattened) leftExpr
            | expr -> (removeParens expr)::flattened

        match functionApplication with
        | AstNode.Expression(SynExpr.App(_, _, _, _, range) as functionApplication) -> 
            Some(flatten [] functionApplication, range)
        | _ -> None

    type Lambda = { Arguments: SynSimplePats list; Body: SynExpr }

    let (|Lambda|_|) lambda = 
        /// A match clause is generated by the compiler for each wildcard argument, 
        /// this function extracts the body expression of the lambda from those statements.
        let rec removeAutoGeneratedMatchesFromLambda = function
            | SynExpr.Match(SequencePointInfoForBinding.NoSequencePointAtInvisibleBinding, 
                            _, 
                            [SynMatchClause.Clause(SynPat.Wild(_), _, expr, _, _)], _, _) ->
                removeAutoGeneratedMatchesFromLambda expr
            | x -> x

        let (|IsCurriedLambda|_|) = function
            | SynExpr.Lambda(_, _, parameter, (SynExpr.Lambda(_) as inner), _) as outer 
                    when outer.Range = inner.Range ->
                Some(parameter, inner)
            | _ -> None

        let rec getLambdaParametersAndExpression parameters = function
            | IsCurriedLambda(parameter, curriedLambda) ->
                getLambdaParametersAndExpression (parameter::parameters) curriedLambda
            | SynExpr.Lambda(_, _, parameter, body, _) ->
                { Arguments = parameter::parameters |> List.rev
                  Body = removeAutoGeneratedMatchesFromLambda body } |> Some
            | _ -> None

        match lambda with
        | AstNode.Expression(SynExpr.Lambda(_, _, _, _, range) as lambda) -> 
            getLambdaParametersAndExpression [] lambda 
            |> Option.map (fun x -> (x, range))
        | _ -> None

    let (|Cons|_|) pattern =
        match pattern with
        | SynPat.LongIdent(LongIdentWithDots([identifier], _), 
                           _, _,
                           Pats([SynPat.Tuple([lhs; rhs], _)]), _, _) 
                when identifier.idText = "op_ColonColon" ->
            Some(lhs, rhs)
        | _ -> None

    type ExtraSyntaxInfo =
        | LambdaArg = 1uy
        | LambdaBody = 2uy
        | Else = 3uy
        | None = 255uy

    [<Struct>]
    type Node(extraInfo:ExtraSyntaxInfo, astNode:AstNode) = 
        member __.ExtraSyntaxInfo = extraInfo
        member __.AstNode = astNode

    let inline private Expression x = Node(ExtraSyntaxInfo.None, Expression x)
    let inline private Pattern x = Node(ExtraSyntaxInfo.None, Pattern x)
    let inline private SimplePattern x = Node(ExtraSyntaxInfo.None, SimplePattern x)
    let inline private SimplePatterns x = Node(ExtraSyntaxInfo.None, SimplePatterns x)
    let inline private ModuleOrNamespace x = Node(ExtraSyntaxInfo.None, ModuleOrNamespace x)
    let inline private ModuleDeclaration x = Node(ExtraSyntaxInfo.None, ModuleDeclaration x)
    let inline private Binding x = Node(ExtraSyntaxInfo.None, Binding x)
    let inline private TypeDefinition x = Node(ExtraSyntaxInfo.None, TypeDefinition x)
    let inline private MemberDefinition x = Node(ExtraSyntaxInfo.None, MemberDefinition x)
    let inline private ComponentInfo x = Node(ExtraSyntaxInfo.None, ComponentInfo x)
    let inline private ExceptionDefinition x = Node(ExtraSyntaxInfo.None, ExceptionDefinition x)
    let inline private ExceptionRepresentation x = Node(ExtraSyntaxInfo.None, ExceptionRepresentation x)
    let inline private UnionCase x = Node(ExtraSyntaxInfo.None, UnionCase x)
    let inline private EnumCase x = Node(ExtraSyntaxInfo.None, EnumCase x)
    let inline private TypeRepresentation x = Node(ExtraSyntaxInfo.None, TypeRepresentation x)
    let inline private TypeSimpleRepresentation x = Node(ExtraSyntaxInfo.None, TypeSimpleRepresentation x)
    let inline private Type x = Node(ExtraSyntaxInfo.None, Type x)
    let inline private Field x = Node(ExtraSyntaxInfo.None, Field x)
    let inline private Match x = Node(ExtraSyntaxInfo.None, Match x)
    let inline private ConstructorArguments x = Node(ExtraSyntaxInfo.None, ConstructorArguments x)
    let inline private Identifier x = Node(ExtraSyntaxInfo.None, Identifier x)

    /// Gets a string literal from the AST.
    let (|StringLiteral|_|) node =
        match node with
        | Expression(SynExpr.Const(SynConst.String(value, _), range)) -> Some(value, range)
        | _ -> None

    let inline private moduleDeclarationChildren node = 
        match node with
        | SynModuleDecl.NestedModule(componentInfo, moduleDeclarations, _, _) -> 
            ComponentInfo componentInfo::(moduleDeclarations |> List.map ModuleDeclaration)
        | SynModuleDecl.Let(_, bindings, _) -> bindings |> List.map Binding
        | SynModuleDecl.DoExpr(_, expression, _) -> [Expression expression]
        | SynModuleDecl.Types(typeDefinitions, _) -> typeDefinitions |> List.map TypeDefinition
        | SynModuleDecl.Exception(exceptionDefinition, _) -> [ExceptionDefinition exceptionDefinition]
        | SynModuleDecl.NamespaceFragment(moduleOrNamespace) -> [ModuleOrNamespace moduleOrNamespace]
        | SynModuleDecl.Open(_)
        | SynModuleDecl.Attributes(_)
        | SynModuleDecl.HashDirective(_)
        | SynModuleDecl.ModuleAbbrev(_) -> []

    let inline private typeChildren node =
        match node with
        | SynType.LongIdentApp(synType, _, _, types, _, _, _)
        | SynType.App(synType, _, types, _, _, _, _) -> 
            Type synType::(types |> List.map Type)
        | SynType.Tuple(types, _) -> 
            types |> List.map (snd >> Type)
        | SynType.Fun(synType, synType1, _)
        | SynType.StaticConstantNamed(synType, synType1, _)
        | SynType.MeasureDivide(synType, synType1, _) -> 
            [Type synType; Type synType1]
        | SynType.Var(_)
        | SynType.Anon(_)
        | SynType.LongIdent(_)
        | SynType.StaticConstant(_) -> []
        | SynType.WithGlobalConstraints(synType, _, _)
        | SynType.HashConstraint(synType, _)
        | SynType.MeasurePower(synType, _, _)
        | SynType.Array(_, synType, _) -> [Type synType]
        | SynType.StaticConstantExpr(expression, _) -> [Expression expression]

    let inline private memberDefinitionChildren node = 
        match node with
        | SynMemberDefn.Member(binding, _) -> [Binding binding]
        | SynMemberDefn.ImplicitCtor(_, _, patterns, _, _) -> patterns |> List.map SimplePattern
        | SynMemberDefn.ImplicitInherit(synType, expression, _, _) -> 
            [Type synType; Expression expression]
        | SynMemberDefn.LetBindings(bindings, _, _, _) -> bindings |> List.map Binding
        | SynMemberDefn.Interface(synType, Some(members), _) -> 
            Type synType::(members |> List.map MemberDefinition)
        | SynMemberDefn.Interface(synType, None, _)
        | SynMemberDefn.Inherit(synType, _, _) -> [Type synType]
        | SynMemberDefn.Open(_)
        | SynMemberDefn.AbstractSlot(_) -> []
        | SynMemberDefn.ValField(field, _) -> [Field field]
        | SynMemberDefn.NestedType(typeDefinition, _, _) -> [TypeDefinition typeDefinition]
        | SynMemberDefn.AutoProperty(_, _, _, Some(synType), _, _, _, _, expression, _, _) -> 
            [Type synType; Expression expression]
        | SynMemberDefn.AutoProperty(_, _, _, None, _, _, _, _, expression, _, _) -> 
            [Expression expression]

    let inline private patternChildren node =
        match node with 
        | SynPat.IsInst(synType, _) -> [Type synType]
        | SynPat.QuoteExpr(expression, _) -> [Expression expression]
        | SynPat.Typed(pattern, synType, _) -> [Pattern pattern; Type synType]
        | SynPat.Or(pattern, pattern1, _) -> [Pattern pattern; Pattern pattern1]
        | SynPat.ArrayOrList(_, patterns, _)
        | SynPat.Tuple(patterns, _)
        | SynPat.Ands(patterns, _) -> patterns |> List.map Pattern
        | SynPat.Attrib(pattern, _, _)
        | SynPat.Named(pattern, _, _, _, _)
        | SynPat.Paren(pattern, _) -> [Pattern pattern]
        | SynPat.Record(patternsAndIdentifier, _) -> patternsAndIdentifier |> List.map (snd >> Pattern)
        | SynPat.Const(_)
        | SynPat.Wild(_)
        | SynPat.FromParseError(_)
        | SynPat.InstanceMember(_)
        | SynPat.DeprecatedCharRange(_)
        | SynPat.Null(_)
        | SynPat.OptionalVal(_) -> []
        | Cons(lhs, rhs) -> 
            [Pattern lhs; Pattern rhs]
        | SynPat.LongIdent(_, _, _, constructorArguments, _, _) -> 
            [ConstructorArguments constructorArguments]

    let inline private expressionChildren node =
        match node with 
        | SynExpr.Paren(expression, _, _, _)
        | SynExpr.DotGet(expression, _, _, _)
        | SynExpr.DotIndexedGet(expression, _, _, _)
        | SynExpr.LongIdentSet(_, expression, _)
        | SynExpr.Do(expression, _)
        | SynExpr.Assert(expression, _)
        | SynExpr.CompExpr(_, _, expression, _)
        | SynExpr.ArrayOrListOfSeqExpr(_, expression, _)
        | SynExpr.AddressOf(_, expression, _, _)
        | SynExpr.InferredDowncast(expression, _)
        | SynExpr.InferredUpcast(expression, _)
        | SynExpr.DoBang(expression, _)
        | SynExpr.Lazy(expression, _)
        | SynExpr.TraitCall(_, _, expression, _)
        | SynExpr.YieldOrReturn(_, expression, _)
        | SynExpr.YieldOrReturnFrom(_, expression, _) -> [Expression expression]
        | SynExpr.Quote(expression, _, expression1, _, _)
        | SynExpr.Sequential(_, _, expression, expression1, _)
        | SynExpr.NamedIndexedPropertySet(_, expression, expression1, _)
        | SynExpr.DotIndexedSet(expression, _, expression1, _, _, _)
        | SynExpr.JoinIn(expression, _, expression1, _)
        | SynExpr.While(_, expression, expression1, _)
        | SynExpr.TryFinally(expression, expression1, _, _, _)
        | SynExpr.DotSet(expression, _, expression1, _) -> 
            [Expression expression; Expression expression1]
        | SynExpr.Typed(expression, synType, _) -> 
            [Expression expression; Type synType]
        | SynExpr.Tuple(expressions, _, _)
        | SynExpr.ArrayOrList(_, expressions, _) -> expressions |> List.map Expression
        | SynExpr.Record(_, Some(expr, _), _, _) -> [Expression expr]
        | SynExpr.Record(_, None, _, _) -> []
        | SynExpr.ObjExpr(synType, _, bindings, _, _, _) -> 
            Type synType::(bindings |> List.map Binding)
        | SynExpr.ImplicitZero(_)
        | SynExpr.Null(_)
        | SynExpr.Const(_)
        | SynExpr.DiscardAfterMissingQualificationAfterDot(_)
        | SynExpr.FromParseError(_)
        | SynExpr.LibraryOnlyILAssembly(_)
        | SynExpr.LibraryOnlyStaticOptimization(_)
        | SynExpr.LibraryOnlyUnionCaseFieldGet(_)
        | SynExpr.LibraryOnlyUnionCaseFieldSet(_)
        | SynExpr.ArbitraryAfterError(_) -> []
        | SynExpr.DotNamedIndexedPropertySet(expression, _, expression1, expression2, _)
        | SynExpr.For(_, _, expression, _, expression1, expression2, _) -> 
            [Expression expression; Expression expression1; Expression expression2]
        | SynExpr.LetOrUseBang(_, _, _, pattern, expression, expression1, _)
        | SynExpr.ForEach(_, _, _, pattern, expression, expression1, _) -> 
            [Pattern pattern; Expression expression; Expression expression1]
        | SynExpr.MatchLambda(_, _, matchClauses, _, _) -> 
            matchClauses |> List.map Match
        | SynExpr.TryWith(expression, _, matchClauses, _, _, _, _)
        | SynExpr.Match(_, expression, matchClauses, _, _) -> 
            Expression expression::(matchClauses |> List.map Match)
        | SynExpr.TypeApp(expression, _, types, _, _, _, _) -> 
            Expression expression::(types |> List.map Type)
        | SynExpr.New(_, synType, expression, _) 
        | SynExpr.TypeTest(expression, synType, _)
        | SynExpr.Upcast(expression, synType, _)
        | SynExpr.Downcast(expression, synType, _) -> 
            [Expression expression; Type synType]
        | SynExpr.LetOrUse(_, _, bindings, expression, _) -> 
            [ yield! bindings |> List.map Binding
              yield Expression expression ]
        | SynExpr.Ident(ident) -> [Identifier([ident.idText])]
        | SynExpr.LongIdent(_, LongIdentWithDots(ident, _), _, _) -> 
            [Identifier(ident |> List.map (fun x -> x.idText))]
        | SynExpr.IfThenElse(cond, body, Some(elseExpr), _, _, _, _) -> 
            [Expression cond; Expression body; Node(ExtraSyntaxInfo.Else, AstNode.Expression elseExpr)]
        | SynExpr.IfThenElse(cond, body, None, _, _, _, _) -> [Expression cond; Expression body]
        | SynExpr.Lambda(_)
        | SynExpr.App(_) -> []

    let inline private typeSimpleRepresentationChildren node =
        match node with 
        | SynTypeDefnSimpleRepr.Union(_, unionCases, _) -> unionCases |> List.map UnionCase
        | SynTypeDefnSimpleRepr.Enum(enumCases, _) -> enumCases |> List.map EnumCase
        | SynTypeDefnSimpleRepr.Record(_, fields, _) -> fields |> List.map Field
        | SynTypeDefnSimpleRepr.TypeAbbrev(_, synType, _) -> [Type synType]
        | SynTypeDefnSimpleRepr.General(_)
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly(_)
        | SynTypeDefnSimpleRepr.None(_) -> []

    let inline private simplePatternsChildren node =
        match node with 
        | SynSimplePats.SimplePats(simplePatterns, _) -> 
            simplePatterns |> List.map SimplePattern
        | SynSimplePats.Typed(simplePatterns, synType, _) -> 
            [SimplePatterns simplePatterns; Type synType]

    let inline private simplePatternChildren node =
        match node with 
        | SynSimplePat.Typed(simplePattern, synType, _) -> 
            [SimplePattern simplePattern; Type synType]
        | SynSimplePat.Attrib(simplePattern, _, _) -> [SimplePattern simplePattern]
        | SynSimplePat.Id(identifier, _, _, _, _, _) -> [Identifier([identifier.idText])]

    let inline private matchChildren node =
        match node with 
        | Clause(pattern, Some(expression), expression1, _, _) -> 
            [Pattern pattern; Expression expression; Expression expression1]
        | Clause(pattern, None, expression1, _, _) -> 
            [Pattern pattern; Expression expression1]

    let inline private constructorArgumentsChildren node =
        match node with 
        | SynConstructorArgs.Pats(patterns) -> 
            patterns |> List.map Pattern
        | SynConstructorArgs.NamePatPairs(namePatterns, _) -> 
            namePatterns |> List.map (snd >> Pattern)

    let inline private typeRepresentationChildren node =
        match node with 
        | SynTypeDefnRepr.ObjectModel(_, members, _) -> 
            members |> List.map MemberDefinition
        | SynTypeDefnRepr.Simple(typeSimpleRepresentation, _) -> 
            [TypeSimpleRepresentation typeSimpleRepresentation]
            
    /// Extracts the child nodes to be visited from a given node.
    let traverseNode node =
        match node with
        | ModuleDeclaration(x) -> moduleDeclarationChildren x
        | ModuleOrNamespace(SynModuleOrNamespace(_, _, moduleDeclarations, _, _, _, _)) -> 
            moduleDeclarations |> List.map ModuleDeclaration
        | Binding(SynBinding.Binding(_, _, _, _, _, _, _, pattern, _, expression, _, _)) -> 
            [Pattern pattern; Expression expression]
        | ExceptionDefinition(ExceptionDefn(exceptionRepresentation, members, _)) -> 
            ExceptionRepresentation exceptionRepresentation::(members |> List.map MemberDefinition)
        | ExceptionRepresentation(ExceptionDefnRepr(_, unionCase, _, _, _, _)) -> [UnionCase unionCase]
        | TypeDefinition(TypeDefn(componentInfo, typeRepresentation, members, _)) -> 
            ComponentInfo componentInfo::
                TypeRepresentation typeRepresentation::
                (members |> List.map MemberDefinition)
        | TypeSimpleRepresentation(x) -> typeSimpleRepresentationChildren x
        | Type(x) -> typeChildren x
        | Match(x) -> matchChildren x
        | MemberDefinition(x) -> memberDefinitionChildren x
        | Field(SynField.Field(_, _, _, synType, _, _, _, _)) -> [Type synType]
        | Pattern(x) -> patternChildren x
        | ConstructorArguments(x) -> constructorArgumentsChildren x
        | SimplePattern(x) -> simplePatternChildren x
        | SimplePatterns(x) -> simplePatternsChildren x
        | InterfaceImplementation(InterfaceImpl(synType, bindings, _)) -> 
            Type synType::(bindings |> List.map Binding)
        | TypeRepresentation(x) -> typeRepresentationChildren x
        | FuncApp(exprs, _) -> exprs |> List.map Expression
        | Lambda({ Arguments = args; Body = body }, _) -> 
            [ yield! args |> List.map (fun arg -> Node(ExtraSyntaxInfo.LambdaArg, AstNode.SimplePatterns arg))
              yield Node(ExtraSyntaxInfo.LambdaBody, AstNode.Expression(body)) ]
        | Expression(x) -> expressionChildren x

        | ComponentInfo(_)
        | EnumCase(_)
        | UnionCase(_)
        | Identifier(_)
        | TypeParameter(_) -> []