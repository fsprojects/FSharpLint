# Hints

### Introduction

The Hints analyser is inspired by [HLint](https://github.com/ndmitchell/hlint). The hints let users easily write their own rules which are matched against linted code and when matched produce a suggestion that the user provides as part of the hint.

Every hint is formed of two parts: the match and the suggestion. Both the match and the suggestion are parsed the same way into ASTs, but they have two different purposes; the match AST is analysed against the code being linted looking for any expressions in the code that match the AST, and if there is a match then the suggestion AST is used to display a suggestion on how the code can be refactored.

### Matching

##### Match Any Expression

Any F# expression can be matched by a variable or wildcard.

* A variable is represented by a single letter e.g. `x`
* A wildcard is represented by the character `_`

Variables and wildcards are seemingly the same, and in terms of matching they are. The key difference is that using a variable lets you refer to it in the suggestion, enabling you to show where the matched expression should be moved within the matched code.

For example if we wanted to match the following:

    not ((4 + 4) >= (x + 77 * (9 * y)))

and suggest the following (which is equivalent):

    (4 + 4) < (x + 77 * (9 * y))

We can use variables here, the expression `(4 + 4)` can be matched by a variable and `(x + 77 * (9 * y))` by another, this is shown below using the variables `a` and `b`.

    [lang=hint]
    not (a >= b) ===> a <  b

##### Match An Identifier

Identifiers in F# code can be matched by using the same identifier in the hint. It's important to note that since single characters are used to represent variables in hints the identifier must be at least 2 characters long.

For example the following rule uses identifiers:

    [lang=hint]
    List.fold (+) 0 ===> List.sum

`List.fold` in the hint will match the same identifier in the code. So if `List.fold` is found anywhere in the F# code being analysed with `(+)` and `0` applied to it then the rule will be matched.

##### Match Literal Constants

Literal constants can be used to match literal constants in the code, the constants in hints are the same format as constants in F#, so for example if you wanted to match `0x4b` you could use `0x4b` in the hint.

Example:

    [lang=hint]
    not true ===> false
	
In the example above the boolean literal `true` is used to match any F# code where `true` is applied to the `not` identifier.

##### Match Function Application and Operators

Matching function application, prefix operators, and infix operators in hints are all done in the same way as how you'd write it in F# e.g.

    [lang=hint]
    not true ===> false
    4 + 4 ===> 8
    ~x ===> someFunc x
	
The first rule above matches `true` (boolean literal) applied to the function `not`, the second matches two literal integers (both `4`) applied to the `+` binary operator, and the third matches an expression applied to the `~` prefix operator.
	
Read the below section titled "Order Of Operations" for specifying the order of application in a hint.

##### Match Lambda Functions

Lambda functions can be matched using the syntax `fun args -> ()` e.g. `fun x y -> x + y`.

The arguments may be either wildcards (`_`) or 'variables' (a single character). The 'variable' arguments have a particular use: they match a lambda that has that argument as an identifier, and then if that 'variable' is used in the body of the lambda in the hint then it will match the argument's identifier in the body of the code.

For example:

    [lang=hint]
    fun x -> x ===> id
	
The above hint will match a lambda that has a single argument which is an identifier and returns that identifier. `fun val -> val` would be matched, whereas `fun val -> ()` would not be matched - to match this you could use the hint: `fun _ -> ()`.

##### Order Of Operations

Generic order of operations can be specified using parentheses. They're described as 'generic' because using parentheses in a hint will also take into account the following operators: `|>`, `||>`, `|||>`, `<|`, `<||`, and `<|||` which are often used to specificy the order of function application.

Below uses parentheses to match `x` applied to `not` and the result of that application applied to `someFunc`.

    [lang=hint]
    someFunc (not x) ===> someOtherFunc x

In F# several operators are commonly used to show the order of function application, for example in F# `someFunc (not x)` could also be written as:

    not x |> someFunc
	
The same code written as a rule `not x |> someFunc` will match the above, but it is matching against the operator so it will not match `someFunc (not x)`. However the rule `someFunc (not x)` will match both.

### EBNF of a Hint

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

### Writing Your Own Hints

Currently to provide your own hints you have to overwrite the default hints with your own using the `Hints `property inside of the `Hints` rule.

For example to make the lint tool run with just the two hints: `not (a =  b) ===> a <> b` and `not (a <> b) ===> a =  b`, you could use the following config file to override existing hints:

    [lang=json]
    {
      "hints": [
        "not (a =  b) ===> a <> b",
        "not (a <> b) ===> a =  b"
      ]
    }

### Flaws

* `===>` is used to split the hints into parts, a hint cannot match this valid F# operator.
* Single letter identifiers are used as variables inside a hint, so attempting to match an identifier that is a single letter is not going to work.
* Operators beginning with `.` e.g. `.*` will have incorrect precedence and as such should not currently be used in hints.

### Future Intentions

* Provide more informative parse errors.
* Allow for adding your own hints and removing select hints rather than always having to override the default with a set of hints.
* Provide support for matching literal lists, literal arrays, literal sequences, tuples, methods, if statements, and match statements.
