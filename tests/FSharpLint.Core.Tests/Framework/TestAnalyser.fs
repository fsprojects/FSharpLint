module TestAnalyser

open NUnit.Framework
open FSharpLint.Framework.Analyser
open FSharp.Compiler.Range

[<TestFixture>]
type TestAnalyser() =
    [<Test>]
    member __.``TryFindTextOfRange gets expected text from given ranges``() = 
        let analyserInfo =
            { AnalyserInfo.Config = { IgnoreFiles = None; Analysers = Map.empty }
              AnalyserInfo.Suggest = ignore
              AnalyserInfo.Text = "123\n345\n678" }

        let textOfRange (line1, col1) (line2, col2) = 
            analyserInfo.TryFindTextOfRange(mkRange "" (mkPos line1 col1) (mkPos line2 col2))
            
        Assert.AreEqual(Some "123", textOfRange (1, 0) (1, 3))
        Assert.AreEqual(Some "345", textOfRange (2, 0) (2, 3))
        Assert.AreEqual(Some "678", textOfRange (3, 0) (3, 3))

        Assert.AreEqual(Some "1", textOfRange (1, 0) (1, 1))
        Assert.AreEqual(Some "8", textOfRange (3, 2) (3, 3))

        Assert.AreEqual(Some "123\n345\n678", textOfRange (1, 0) (3, 3))