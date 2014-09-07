Hints are rules that allow an expression to be matched and also suggest a refactor into a simpler expression. For example the expression `not ((x * y) = z)` could be refactored into a simpler `(x * y) <> z`.

####A quick walk through of the structure of a hint

Let's say we have a file named `TestHints.fs` and inside this file on line 4 is the following code:

`    not ((x * y) = z) |> ignore
`

The example we use here will be to write a hint that'll suggest the user to refactor the above into the equivalent:

`    (x * y) <> z |> ignore
`

A hint is split by `===>` into two parts, the first part being the pattern of the expression to match, the second part being the suggested pattern that any code matching the first part should be refactored to:

`match expression ===> suggest expression`

To write an expression "pattern" we can make use of the following: variables, wildcards, identifiers, literal constants, infix operators, prefix operators.

Here we want to match: function application of `not` with an expression that is the `=` operator applied to any two expressions. The hint can be seen below:

`not (a = b) ===> a <> b`

In the hint above, `a` and `b` are variables; variables are single letter identifiers - variables match *any* expression. Wildcards `_` can also be used to match any expression, however using variables allows you to show the user how the expressions should be changed to refactor the code (if the expression is not wanted in the refactored code then that would be perfect used of a wildcard).

Output of the lint tool:

```
not (a=b) can be refactored into a<>b
Error in file ..\..\..\FSharpLint.FunctionalTest.TestedProject\TestHints.fs on line 6 starting at column 4 
    not ((x * y) = z) |> ignore
    ^
```

####Current limitations

Because of the use of `===>` to split the rules into parts, a rule cannot be written to match this valid F# operator.

Operators beginning with `.` e.g. `.*` will have incorrect precedence and as such should not currently be used in hints.

Single letter identifiers are used as variables inside a hint, so attempting to match an identifier that is a single letter is not going to work.

####Future ideas

Next concerns are matching if statements and method calls, and applying constraints to variables e.g. x must be a literal decimal constant.