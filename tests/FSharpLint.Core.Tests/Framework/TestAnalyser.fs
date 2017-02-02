// FSharpLint, a linter for F#.
// Copyright (C) 2016 Matthew Mcveigh
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

module TestAnalyser

open System
open NUnit.Framework
open FSharpLint.Framework.Analyser
open Microsoft.FSharp.Compiler.Range

[<TestFixture>]
type TestAnalyser() =
    [<Test>]
    member __.``TryFindTextOfRange gets expected text from given ranges``() = 
        let analyserInfo =
            { AnalyserInfo.Config = { IgnoreFiles = None; Analysers = Map.empty }
              AnalyserInfo.FSharpVersion = Version()
              AnalyserInfo.Suggest = ignore
              AnalyserInfo.SuggestAsync = ignore
              AnalyserInfo.Text = "123\n345\n678" }

        let textOfRange (line1, col1) (line2, col2) = 
            analyserInfo.TryFindTextOfRange(mkRange "" (mkPos line1 col1) (mkPos line2 col2))
            
        Assert.AreEqual(Some "123", textOfRange (1, 0) (1, 3))
        Assert.AreEqual(Some "345", textOfRange (2, 0) (2, 3))
        Assert.AreEqual(Some "678", textOfRange (3, 0) (3, 3))

        Assert.AreEqual(Some "1", textOfRange (1, 0) (1, 1))
        Assert.AreEqual(Some "8", textOfRange (3, 2) (3, 3))

        Assert.AreEqual(Some "123\n345\n678", textOfRange (1, 0) (3, 3))