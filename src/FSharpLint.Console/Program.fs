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

module program =

    let parseLiteralString () = 
        let input = """
namespace goat
module MyModule1 =

    // Define a type. 
    type myClass() =
      member this.F() = 100

module MyModule2 =
    // Define type extension. 
    type MyModule1.myClass with 
       member this.goat() = 200

   let function1 (obj1: MyModule1.myClass) =
      // Call an ordinary method.
      printfn "%d" (obj1.F())
      // Call the extension method.
      printfn "%d" (obj1.goat())"""

        let postError range error =
            ErrorHandling.errorHandler.Post(
                {
                    Info = error
                    Range = range
                    Input = input
                })

        let visitors = [
            FSharpLint.Rules.NameConventions.visitor postError
            FSharpLint.Rules.FavourIgnoreOverLetWild.visitor postError
            FSharpLint.Rules.FunctionParametersLength.visitor postError
        ]

        try
            FSharpLint.Framework.Ast.parseInput input visitors |> ignore
        with 
            | :? FSharpLint.Framework.Ast.ParseException as e -> 
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