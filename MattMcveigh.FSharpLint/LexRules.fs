namespace MattMcveigh.FSharpLint

module LexRules =

    open Microsoft.FSharp.Compiler.SourceCodeServices
    open LexRuleMatching

    let anyOperator =
        {
            tokenKind = TokenCharKind.Operator
            toMatch = Anything
        }

    let delimiter =
        {
            tokenKind = TokenCharKind.Delimiter
            toMatch = Anything
        }

    let spaceCharacter =
        {
            tokenKind = TokenCharKind.WhiteSpace
            toMatch = StringMatch " "
        }

    let operatorWhitespaceRule = [ 
        Not (One (Token spaceCharacter))
        One (Token spaceCharacter)
        One (Token anyOperator)
        One (Token spaceCharacter)
        Not (One (Token spaceCharacter))
    ]

    let delimiterWhitespaceRule = [ 
        One (Token delimiter)
        One (Token spaceCharacter)
        Not (One (Token spaceCharacter))
    ]

    let rules = [
        operatorWhitespaceRule
        delimiterWhitespaceRule
    ]