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

    open Microsoft.FSharp.Compiler.Ast
    open ErrorHandling
    open Ast
    open AstVisitorBase
        
    [<EntryPoint>]
    let main argv = 
        let input = """
          namespace MattMcveigh.dogharpLint

          module Program =
            type dog = { cat: int; dog: string }

            type guinea =
            | Some
            | None
                member this.ToDog() =
                    ()

            type enum =
            | Cat = 1
            | Dog = 3

            exception MyError of string

            let foo() = 
              let msg = "Hello world"
              let (cat,dog) = 0, 1
              if true then 
                printfn "%s" msg 
              match true with
              | true as dog -> ()
              | _ -> ()
              
              match true with
              | dog -> ()"""
              

        let identError (identifier:Ident) error =
            errorHandler.Post(
                {
                    info = "Invalid identifier " + identifier.idText
                    range = identifier.idRange
                    input = input
                })

        let visitor = { new AstVisitorBase() with
                member this.VisitModuleOrNamespace(identifier, _, _, _, _, _, range) = 
                    identError identifier.Head range
                    Continue

                member this.VisitUnionCase(_, identifier, _, _, _, range) = 
                    identError identifier range
                    Continue

                member this.VisitNamedPattern(_, identifier, _, _, range) = 
                    identError identifier range
                    Continue

                member this.VisitIdPattern(identifier, range) = 
                    identError identifier range
                    Continue

                member this.VisitField(_, identifier, _, _, _, range) = 
                    identifier |> Option.iter (fun ident -> identError ident range)
                    Continue

                member this.VisitEnumCase(_, identifier, _, _, range) = 
                    identError identifier range
                    Continue

                member this.VisitComponentInfo(_, _, _, identifier, _, _, range) = 
                    identError identifier.Head range
                    Continue
            }

        parse "/home/user/Dog.test.fsx" input [visitor]

        System.Console.ReadKey() |> ignore

        0