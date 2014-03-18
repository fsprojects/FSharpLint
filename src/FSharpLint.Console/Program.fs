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

namespace FSharpLint.Console

module Program =

    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration

    let parseLiteralString () = 
        let input = """
module goat

let dog = fun x ->
    ()

type Goat =
    | Dog

    member this.Cat() = ()

type Class() =
    let mutable value = 10
let Goat2() =
    let Cat = 5

    Cat

let Dog = 7"""

        let postError range error =
            ErrorHandling.errorHandler.Post(
                {
                    Info = error
                    Range = range
                    Input = input
                })

        let config = loadDefaultConfiguration()

        let visitorInfo = 
            {
                Config = config
                PostError = postError
            }

        let visitors = [
            FSharpLint.Rules.NameConventions.visitor visitorInfo
            FSharpLint.Rules.FavourIgnoreOverLetWild.visitor visitorInfo
            FSharpLint.Rules.FunctionParametersLength.visitor visitorInfo
            FSharpLint.Rules.XmlDocumentation.visitor visitorInfo
            FSharpLint.Rules.SourceLength.visitor visitorInfo
        ]
         
        try
            parseInput input visitors |> ignore
        with 
            | :? ParseException as e -> 
                System.Console.WriteLine(e.Message)

    let help () =
        System.Console.WriteLine("Use -f followed by the absolute path of the .fsproj \
        file of the project to lint to run the tool.")
    
    [<EntryPoint>]
    let main argv = 
        parseLiteralString()
        System.Console.ReadKey() |> ignore

        (*
        if argv.Length < 2 then
            help()
        else
            match argv.[0] with
            | "-f" -> 
                ProjectFile.parseProject argv.[1]
                System.Console.WriteLine("Finished.")
                System.Console.ReadKey() |> ignore
            | _ -> help()
        *)

        0