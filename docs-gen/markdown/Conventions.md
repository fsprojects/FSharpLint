# Conventions

Set of rules that check F# coding conventions.

## Sub-groups

* [Name Conventions](NameConventions.html)
* [Binding](Binding.html)
* [Function Reimplementation](FunctionReimplementation.html)
* [Number of Items](NumberOfItems.html)
* [Source Length](SourceLength.html)

### Rules

#### RecursiveAsyncFunction - FS0013

##### Cause

Recursive async function ending in `do!` instead of `return!` is unsafe.

##### Rationale

If you end your recursive function with `do!` instead of `return!`, the compiler
cannot perform tail-call optimization.

##### How To Fix

Use `return!` instead of `do!`

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default false)

#### RedundantNewKeyword - FS0014

##### Cause

Using `new` to instantiate a type which does not implement `IDisposable`.

##### Rationale

`new` is redundant for insantiating types which do not implement `IDisposable`.

##### How To Fix

Remove `new` keyword.

##### Rule Settings

`enabled` - A boolean property that can enable and disable this rule. (Default true)

#### NestedStatements - FS0015

Single rule that checks code is not more deeply nested than a configurable depth.

##### Cause

A statement is nested deeper than a configurable depth, for example if `MaxItems` was set to 8 (the default) then the following code would cause an error:

    let dog =
		if true then 									// Depth 1
			if true then								// Depth 2
				if true then							// Depth 3
					if true then						// Depth 4
						if true then					// Depth 5
							if true then				// Depth 6
								if true then			// Depth 7
									if true then		// Depth 8
										()		// Depth 9!!

##### Rationale

When code becomes too deeply nested it becomes more difficult to read and understand, try to refactor nested code out into functions. 

##### How To Fix

Reduce the depth of the deepest statement, e.g. to fix the example in the "Cause" section you'd take out on level of depth:

    let dog =
		if true then 									// Depth 1
			if true then								// Depth 2
				if true then							// Depth 3
					if true then						// Depth 4
						if true then					// Depth 5
							if true then				// Depth 6
								if true then			// Depth 7
									()			// Depth 8

### Analyser Settings

`enabled` - A boolean property that can enable and disable this rule. (Default true)
`depth` - An integer property that specifies the max depth of a statement. (Default 8)
