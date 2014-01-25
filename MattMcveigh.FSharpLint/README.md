# FSharpLint

## Source Code Rules, Lexical Analysis Rules, Abstract Syntax Tree Rules

Different rules require different levels of information to determine whether the source code passes the rules.


The linter will have 3 different levels: 


1. Rules that analyse raw source code.
2. Rules that analyse tokens.
3. Rules that analyse an abstract syntax tree.


Examples of rules where the different levels could be used:


⋅⋅* Level 1 could be used by a rule that gives warnings when a line of source code contains too many characters.
⋅⋅* Level 2 could be used by a rule that gives warnings when a binary operator does not have whitespace on both sides of it.
⋅⋅* Level 3 could be used by a rule that gives warnings when there's a redudant else if statement.


The advantage of this is that it will be much simpler to write rules for their appropriate levels than say trying to write all 
rules against an abstract syntax tree. It also means the different levels can run in parallel.