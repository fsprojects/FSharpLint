#Hints

###Introduction

The Hints analyser is inspired by [HLint](https://github.com/ndmitchell/hlint). The hints let users easily write their own rules which are matched against linted code and when matched produce a suggestion that the user provides as part of the hint.

Every hint is formed of two parts: the match and the suggestion. Both the match and the suggestion are parsed the same way into ASTs, but they have two different purposes; the match AST is analysed against the code being linted looking for any expressions in the code that match the AST, and if there is a match then the suggestion AST is used to display a suggestion on how the code can be refactored.

###Matching

#####Match Any Expression
todo: variable
todo: wildcard

#####Match Identifier
todo: identifier

#####Match Literal Constants
todo: constants

#####Match Function Application and Operators
todo: function application
todo: prefix operator
todo: infix operator

#####Match Lambda Functions
todo: lambda

#####Order Of Operations
todo: parentheses

###EBNF of a Hint

This is incomplete - currently missing a few of the more detailed rules e.g. `uint32` and `infix-operator`, for these I'd recommend looking them up in the EBNF for F# as that's what they will be based upon.

    [lang=ebnf]
    whitespace = " " | "\t" | "\n" | "\r\n" | "\r";

    spaces = [{whitespace}];

    spaces1 = whitespace, [{whitespace}];

    bool = "true" | "false";

    unit = "(", [spaces], ")";

    constant = bool
                | unit
                | character
                | literal-string
                | verbatim-string
                | byte-char
                | byte-array
                | verbatim-byte-array
                | triple-quoted-string
                | sbyte
                | byte
                | int16
                | uint16
                | uint32
                | native-int
                | unative-int
                | int64
                | uint64
                | single
                | big-num
                | decimal
                | double
                | int32;

    parentheses = "(" expression ")";

    wildcard = "_";

    variable = letter, -letter;

    ident-start-char = "_" | letter;

    ident-char = letter | digit | "'" | "_" ;

    ident-text = ident-start-char, {ident-char};

    ident = ident-text | ("``", {(-("`" | "\n" | "\r" | "\t")) | (("`"), -("`" | "\n" | "\r" | "\t"))}, "``");

    ident-or-op = ident | ("(", spaces, operator, spaces, ")");

    long-ident = {ident, "."} | ident;

    long-ident-or-op = ident, {".", ident}, [".", ident-or-op]
                        | ident-or-op
                        | long-ident;

    application = constant
                    | variable
                    | wildcard
                    | long-ident-or-op
                    | parentheses;

    function-application = long-ident-or-op, identifier, spaces, {application, spaces}, [application, spaces];

    prefix-expr = prefix-operator, spaces, expression

    infix-expr = expression, spaces, infix-operator, spaces, expression

    identifier = long-ident-or-op -letter;

    argument-variable = letter;

    argument-wildcard = "_";

    lambda-arguments = [{(argument-variable | argument-wildcard), spaces1}], 
                            (argument-variable | argument-wildcard), [spaces]

    lambda = "fun", spaces1, lambda-arguments, "->", spaces, expression;

    expression = spaces, (constant | lambda | variable | wildcard | function-application
                          | identifier | parentheses | infix-expr | prefix-expr), spaces;

    suggestion = expression;

    match = expression;

    hint = match, spaces, "===>", spaces, suggestion;

###Writing Your Own Hints

Currently to provide your own hints you have to overwrite the default hints with your own using the `Hints `property inside of the `Hints` rule.

For example to make the lint tool run with just the two hints: `not (a =  b) ===> a <> b` and `not (a <> b) ===> a =  b`, you could use the following config file to override existing hints:

    [lang=xml]
    <?xml version="1.0" encoding="utf-8"?>
    <FSharpLintSettings>
      <Analysers>
        <Analyser AnalyserId="FSharpLint.Hints">
          <Rules>
            <Rule Name="Hints">
              <RuleSettings>
                <Property name="Hints">
                  <![CDATA[
                    not (a =  b) ===> a <> b
                    not (a <> b) ===> a =  b
                  ]]>
                </Property>
              </RuleSettings>
            </Rule>
          </Rules>
          <AnalyserSettings>
            <Property name="Enabled">True</Property>
          </AnalyserSettings>
        </Analyser>
      </Analysers>
    </FSharpLintSettings>

###Flaws

* `===>` is used to split the hints into parts, a hint cannot match this valid F# operator.
* Single letter identifiers are used as variables inside a hint, so attempting to match an identifier that is a single letter is not going to work.
* Operators beginning with `.` e.g. `.*` will have incorrect precedence and as such should not currently be used in hints.

###Future Intentions

* Provide more informative parse errors.
* Allow for adding your own hints and removing select hints rather than always having to override the default with a set of hints.
* Provide support for matching literal lists, literal arrays, literal sequences, tuples, methods, if statements, and match statements
* Provide support for 

###Analyser Settings

`Enabled` - A boolean property that can enable and disable this analyser. (Default True)