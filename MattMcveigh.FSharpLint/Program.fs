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

    open Ast
    open ErrorHandling
    open NameConventionRules
        
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
                for i in 1..10 do 
                  ()
                let Cats = 8
                Cats

            type sizeType = uint32

            type Point2D =
              struct 
                val X: float
                val Y: float
                new(x: float, y: float) = { X = x; Y = y }
              end

            type IPrintable =
              abstract member Print : unit -> unit

            [<AbstractClass>]
            type Shape2D(x0 : float, y0 : float) =
              let mutable x, y = x0, y0
              let mutable rotAngle = 0.0

              // These properties are not declared abstract. They 
              // cannot be overriden. 
              member this.CenterX with get() = x and set xval = x <- xval
              member this.CenterY with get() = y and set yval = y <- yval

              // These properties are abstract, and no default implementation 
              // is provided. Non-abstract derived classes must implement these. 
              abstract Area : float with get
              abstract Perimeter : float  with get
              abstract Name : string with get

              // This method is not declared abstract. It cannot be 
              // overriden. 
              member this.Move dx dy =
                x <- x + dx
                y <- y + dy

              abstract member Rotate: float -> unit
              default this.Rotate(angle) = rotAngle <- rotAngle + angle

            type SomeClass1(x: int, y: float) =
              interface IPrintable with 
                member this.Print() = printfn "%d %f" x y

            let makePrintable(x: int, y: float) =
              { new IPrintable with 
                  member this.Print() = printfn "%d %f" x y }

            type MyClass2(dataIn) as self =
              let data = dataIn
              do
                self.PrintMessage()
              member this.PrintMessage() =
                printf "Creating MyClass2 with Data %d" data

            type Folder(pathIn: string) =
              let path = pathIn
              let filenameArray : string array = Directory.GetFiles(path)
              member this.FileArray = Array.map (fun elem -> new File(elem, this)) filenameArray

            type enum =
            | Cat = 1
            | Dog = 3

            type Delegate2 = delegate of int * int -> int

            exception MyError of string

            let foo() = 
              let Msg = "Hello world"
              let (Cat,Dog) = 0, 1
              if true then 
                printfn "%s" msg 
              match true with
              | true as Dog -> ()
              | _ -> ()
              
              match true with
              | Dog -> ()"""

        

        let postError range error =
            errorHandler.Post(
                {
                    info = error
                    range = range
                    input = input
                })

        let visitor = namingConventionVisitor postError

        parse "/home/user/Dog.test.fsx" input [visitor]

        System.Console.ReadKey() |> ignore

        0