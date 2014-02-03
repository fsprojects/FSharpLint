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

    let visitModulesAndNamespaces modulesOrNamespaces input =
        let isValidName (identifier:Ident) = isPascalCase identifier.idText

        let toError isModule (identifier:Ident) range =
            let firstLine = errorInfoLine range input
            let moduleOrNamespace = if isModule then "module" else "namespace"
            let secondLine = String.Format("Expected {0} {1} to be PascalCase", moduleOrNamespace, identifier.idText)
            firstLine + Environment.NewLine + secondLine

        let getErrors moduleOrNamespace =
            let (SynModuleOrNamespace(longIndentifier, isModule, declarations, _, _, _, range)) = moduleOrNamespace

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