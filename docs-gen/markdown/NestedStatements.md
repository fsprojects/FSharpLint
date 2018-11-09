# NestedStatements

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

`Enabled` - A boolean property that can enable and disable this rule. (Default true)
`Depth` - An integer property that specifies the max depth of a statement. (Default 8)