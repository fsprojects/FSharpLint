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

    let parseLiteralString () = 
        let input = """
module goat

type Point3D =
    struct 
        val mutable _x: float
        val y: float
        val z: float
    end 

let ((_)) = ()

let _, a = 0, 1

//for I = 10 downto 1 do System.Console.Write(I)

for I in 1..10 do System.Console.Write(I)

let functionCat meow dog (m, d, g) e o w = ()

type MyClass(x) =
    new() = MyClass(0)
        """

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

        FSharpLint.Framework.Ast.parseInput input visitors |> ignore
    
    [<EntryPoint>]
    let main argv = 
        //ProjectFile.parseProject @"C:\Users\Matt\Documents\GitHub\FSharpLint\src\FSharpLint.Console\FSharpLint.Console.fsproj"
        ProjectFile.parseProject @"C:\Users\Matt\Documents\GitHub\FSharpLint\src\FSharpLint.Framework\FSharpLint.Framework.fsproj"

        System.Console.ReadKey() |> ignore

        0