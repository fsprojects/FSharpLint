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
    open FSharpLint.Application.ErrorHandling

    let parseLiteralString () = 
        let input = """
module goat

type Goat =
    | Dog
    | Fart

let meow a b c d e f g h i = 
    match a with
    | Dog -> ()
    | Fart -> ()"""

        let postError range error =
            errorHandler.Post(
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
            | ParseException(e)
            | ConfigurationException(e) ->
                System.Console.WriteLine(e)

    let help () =
        System.Console.WriteLine("Use -f followed by the absolute path of the .fsproj \
        file of the project to lint to run the tool.")
    
    [<EntryPoint>]
    let main argv = 
        (*
        parseLiteralString()
        System.Console.ReadKey() |> ignore

        *)

        let argv = [| "-f"; @"C:\Users\matthewm\Documents\GitHub\FSharp.Data\src\FSharp.Data.fsproj" |]

        if argv.Length < 2 then
            help()
        else
            match argv.[0] with
            | "-f" -> 
                let finishEarly = System.Func<_>(fun _ -> false)
                let action = System.Action<_>(fun _ -> ())

                ignore <|
                    FSharpLint.Application.ProjectFile.parseProject(finishEarly, argv.[1], action, action)
                System.Console.WriteLine("Finished.")
                System.Console.ReadKey() |> ignore
            | _ -> help()

        0