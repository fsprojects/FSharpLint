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

module TestConfiguration

open NUnit.Framework
open FSharpLint.Framework.Configuration

[<TestFixture>]
type TestConfiguration() =
    [<Test>]
    member self.OverwriteMap() = 
        let mapToBeOverwrited = [ (1,"1"); (2,"2"); (3,"3"); (4,"5") ] |> Map.ofList

        let map = [ (2,"5"); (4,"1");  ] |> Map.ofList

        let expectedMap = [ (1,"1"); (2,"5"); (3,"3"); (4,"1")  ] |> Map.ofList

        Assert.AreEqual(expectedMap, overwriteMap mapToBeOverwrited map (fun _ x -> x))