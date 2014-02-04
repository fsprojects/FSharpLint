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

namespace MattMcveigh.FSharpLint

module Program =

    open System
    open System.Text.RegularExpressions
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Tokeniser

    let checker = InteractiveChecker.Create()

    let getUntypedTree (file, input) = 
        let projOptions = checker.GetProjectOptionsFromScript(file, input)
        let parseFileResults = checker.ParseFileInProject(file, input, projOptions)
        match parseFileResults.ParseTree with
        | Some tree -> tree
        | None -> failwith "Something went wrong during parsing!"

    let isPascalCase str = Regex.Match(str, @"^[A-Z]([a-z]|[A-Z]|\d)*").Success

    let isCamelCase str = Regex.Match(str, @"^[a-z]([a-z]|[A-Z]|\d)*").Success

    let errorInfoLine (range:range) (input:string) =
        let errorenousLine = input.Split([|'\n'|]).[range.StartLine]
        let firstLine = String.Format("Error in file {0} on line {1} starting at column {2}", range.FileName, range.StartLine, range.StartColumn)
        let highlightColumnLine = 
            errorenousLine 
                |> Seq.mapi (fun i x -> if i = range.StartColumn then "^" else " ")
                |> Seq.reduce (+)

        firstLine + Environment.NewLine + errorenousLine + Environment.NewLine + highlightColumnLine

    let visitType = function
        | SynType.Fun(_, _, _) -> ()
        | _ -> ()

    let rec visitPattern = function
        | SynPat.Named(pat, name, _, _, _) ->
            visitPattern pat
            printfn "%s" name.idText
        | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) ->
            let names = String.concat "." [ for i in ident -> i.idText ]
            printfn "%s" names
        | SynPat.Const(_,m) -> ()
        | SynPat.Or (pattern1, pattern2, m) -> 
            visitPattern pattern1
            visitPattern pattern2
        | SynPat.Ands (patterns,m)
        | SynPat.ArrayOrList(_,patterns,m)
        | SynPat.Tuple (patterns,m) -> 
            for pattern in patterns do visitPattern pattern
        | SynPat.Typed(pattern, synType, _) -> 
            visitPattern pattern
            visitType synType
        | SynPat.Attrib(_,_,m) 
        | SynPat.Record (_,m) 
        | SynPat.IsInst (_,m) 
        | SynPat.QuoteExpr (_,m)
        | SynPat.InstanceMember(_,_,_,_,m) -> ()
        | SynPat.OptionalVal(identifier,m) -> printfn "%s" identifier.idText
        | SynPat.Paren(_,m) -> ()
        | _ -> ()

    let rec visitExpression = function
        | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
            visitExpression cond
            visitExpression trueBranch
            falseBranchOpt |> Option.iter visitExpression 
        | SynExpr.LetOrUse(_, _, bindings, body, _) ->
            for binding in bindings do
                let (Binding(_, _, _, _, _, _, _, pat, _, init, _, _)) = binding
                visitPattern pat 
                visitExpression init
                visitExpression body
        | SynExpr.Paren(_,_,_,m) 
        | SynExpr.Quote(_,_,_,_,m) 
        | SynExpr.Const(_,m) 
        | SynExpr.Typed (_,_,m)
        | SynExpr.Tuple (_,_,m)
        | SynExpr.ArrayOrList (_,_,m)
        | SynExpr.Record (_,_,_,m)
        | SynExpr.New (_,_,_,m)
        | SynExpr.ObjExpr (_,_,_,_,_,m)
        | SynExpr.While (_,_,_,m)
        | SynExpr.For (_,_,_,_,_,_,m)
        | SynExpr.ForEach (_,_,_,_,_,_,m)
        | SynExpr.CompExpr (_,_,_,m)
        | SynExpr.ArrayOrListOfSeqExpr (_,_,m)
        | SynExpr.Lambda (_,_,_,_,m)
        | SynExpr.Match (_,_,_,_,m)
        | SynExpr.MatchLambda (_,_,_,_,m)
        | SynExpr.Do (_,m)
        | SynExpr.Assert (_,m)
        | SynExpr.App (_,_,_,_,m)
        | SynExpr.TypeApp (_,_,_,_,_,_,m)
        | SynExpr.TryWith (_,_,_,_,m,_,_)
        | SynExpr.TryFinally (_,_,m,_,_)
        | SynExpr.Sequential (_,_,_,_,m)
        | SynExpr.ArbitraryAfterError(_,m)
        | SynExpr.FromParseError (_,m) 
        | SynExpr.DiscardAfterMissingQualificationAfterDot (_,m) 
        | SynExpr.LongIdent (_,_,_,m)
        | SynExpr.LongIdentSet (_,_,m)
        | SynExpr.NamedIndexedPropertySet (_,_,_,m)
        | SynExpr.DotIndexedGet (_,_,_,m)
        | SynExpr.DotIndexedSet (_,_,_,_,_,m)
        | SynExpr.DotGet (_,_,_,m)
        | SynExpr.DotSet (_,_,_,m)
        | SynExpr.DotNamedIndexedPropertySet (_,_,_,_,m)
        | SynExpr.LibraryOnlyUnionCaseFieldGet (_,_,_,m)
        | SynExpr.LibraryOnlyUnionCaseFieldSet (_,_,_,_,m)
        | SynExpr.LibraryOnlyILAssembly (_,_,_,_,m)
        | SynExpr.LibraryOnlyStaticOptimization (_,_,_,m)
        | SynExpr.TypeTest (_,_,m)
        | SynExpr.Upcast (_,_,m)
        | SynExpr.AddressOf (_,_,_,m)
        | SynExpr.Downcast (_,_,m)
        | SynExpr.JoinIn (_,_,_,m)
        | SynExpr.InferredUpcast (_,m)
        | SynExpr.InferredDowncast (_,m)
        | SynExpr.Null m
        | SynExpr.Lazy (_, m)
        | SynExpr.TraitCall(_,_,_,m)
        | SynExpr.ImplicitZero (m)
        | SynExpr.YieldOrReturn (_,_,m)
        | SynExpr.YieldOrReturnFrom (_,_,m)
        | SynExpr.LetOrUseBang  (_,_,_,_,_,_,m)
        | SynExpr.DoBang  (_,m) -> ()
        | SynExpr.Ident id -> ()

    let visitSimpleTypeDef = function
    | SynTypeDefnSimpleRepr.Union(_, unionCases, _) -> 
        for unionCase in unionCases do
            match unionCase with
            | SynUnionCase.UnionCase(_, identifier, _, _, _, _) -> ()
    | SynTypeDefnSimpleRepr.Record(_, fields, _) -> 
        for field in fields do
            match field with
            | SynField.Field(_, _, identifier, _, _, _, _, _) -> ()
    | _ -> ()

    let visitTypeDef = function
    | SynTypeDefn.TypeDefn(a, typeDefinition, members, _) -> 
        match typeDefinition with
        | SynTypeDefnRepr.Simple(typedefintion, _) -> visitSimpleTypeDef typedefintion
        | _ -> ()

    let rec visitDeclarations decls = 
        for declaration in decls do
            match declaration with
            | SynModuleDecl.Let(_, bindings, _) ->
                for binding in bindings do
                    let (Binding(_, _, _, _, _, _, _, pattern, _, body, _, _)) = binding
                    visitPattern pattern 
                    visitExpression body         
            | SynModuleDecl.NestedModule(_, declrs, _, _) -> visitDeclarations declrs
            | SynModuleDecl.ModuleAbbrev(identifier, longIdentifier, _) -> ()
            | SynModuleDecl.DoExpr(_, expression, _) -> ()
            | SynModuleDecl.Types(types, _) -> 
                for typeDefinition in types do visitTypeDef typeDefinition
            | SynModuleDecl.Exception(_, _) -> ()
            | SynModuleDecl.NamespaceFragment(SynModuleOrNamespace(_,_,_,_,_,_,_)) -> ()
            | SynModuleDecl.Attributes(_, _) -> ()
            | SynModuleDecl.HashDirective(_, _) -> ()
            | SynModuleDecl.Open(_, _) -> ()

    let visitModulesAndNamespaces modulesOrNamespaces input =
        let isValidName (identifier:Ident) = isPascalCase identifier.idText

        let toError isModule (identifier:Ident) range =
            let firstLine = errorInfoLine range input
            let moduleOrNamespace = if isModule then "module" else "namespace"
            let secondLine = String.Format("Expected {0} {1} to be PascalCase", moduleOrNamespace, identifier.idText)
            firstLine + Environment.NewLine + secondLine

        let getErrors moduleOrNamespace =
            let (SynModuleOrNamespace(longIndentifier, isModule, declarations, _, _, _, range)) = moduleOrNamespace

            visitDeclarations declarations

            longIndentifier |> List.choose (fun identifier ->
                if isValidName identifier then
                    None
                else
                    Some(toError isModule identifier range))

        modulesOrNamespaces |> List.map getErrors
        
    [<EntryPoint>]
    let main argv = 
        let input = """
          namespace MattMcveigh.dogharpLint

          module Program =
            type dog = { cat: int; poo: string }

            type guinea =
            | Some
            | None

            let foo() = 
              let msg = "Hello world"
              if true then 
                printfn "%s" msg """

        let errors = 
            match getUntypedTree("/home/user/Dog.test.fsx", input)  with
            | ParsedInput.ImplFile(implFile) ->
                // Extract declarations and walk over them
                let (ParsedImplFileInput(fn, script, name, _, _, modules, _)) = implFile
                visitModulesAndNamespaces modules input
            | _ -> failwith "F# Interface file (*.fsi) not supported."

        errors |> List.iter System.Console.WriteLine

        System.Console.ReadKey() |> ignore

        0