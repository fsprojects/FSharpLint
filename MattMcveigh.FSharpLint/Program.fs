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

    open Tokeniser

    [<EntryPoint>]
    let main argv = 
        let tokens = tokenise "" [ "let dog = 7 + 8"; "let meow = function | x -> ()" ]

        tokens |> List.iteri (fun i list -> 
            System.Console.Write("\n\nLine " + (i + 1).ToString() + ":\n")
            List.iter (fun tok -> System.Console.Write(tok.ToString() + " ")) list)

        System.Console.ReadKey() |> ignore

        0