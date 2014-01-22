namespace MattMcveigh.FSharpLint

module LexRuleMatching =

    open Microsoft.FSharp.Compiler.SourceCodeServices
    
    type Match =
    | RegexMatch of string
    | StringMatch of string
    | Anything

    type MatchToken = 
        {
            tokenKind: TokenCharKind
            toMatch: Match
        }
        
    type LexRule =
    | Not of LexRule
    | One of LexRule
    | Optional of LexRule
    | Repetition of LexRule
    | Alternation of LexRule
    | Rule of LexRule list
    | Token of MatchToken
    
    let rec findBrokenRules notifyRuleBroken rules = function
    | token::tokens -> 
        

        findBrokenRules notifyRuleBroken rules tokens
    | [] -> ()