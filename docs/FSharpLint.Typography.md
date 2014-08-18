#Typography

Set of rules that analyse the text in a file.

###Analyser Settings

`Enabled` - A boolean property that can enable and disable this analyser. (Default false)

###Rules

|Rules|
|---|
|[MaxLinesInFile](#maxlinesinfile)|
|[MaxCharactersOnLine](#maxcharactersonline)|
|[NoTabCharacters](#notabcharacters)|
|[TrailingNewLineInFile](#trailingnewlineinfile)|
|[TrailingWhitespaceOnLine](#trailingwhitespaceonline)|

####MaxLinesInFile

#####Cause

More than a configurable number of lines were found in a file.

#####Rationale

Too many lines in a file indicate it's becoming too complex.

#####How To Fix

Refactor to extract code out into another file.

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)

####MaxCharactersOnLine

#####Cause

More than a configurable number of characters were on a single line.

#####Rationale

Too many characters on a single line make code harder to read by forcing the reader to scroll horizontally.

#####How To Fix

Break the line up into multiple lines.

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)

####NoTabCharacters

#####Cause

A tab character was found in a file.

#####Rationale

It's best practice to use spaces to indent code rather than tabs, this is because tabs have different widths on different platforms.

#####How To Fix

Replace the tab with spaces.

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)

####TrailingNewLineInFile

#####Cause

A new line was found at the end of a file.

#####Rationale

Pointless whitespace.

#####How To Fix

Remove any new lines at the end of a file.

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)

####TrailingWhitespaceOnLine

#####Cause

Whitespace was found at the end of a line.

#####Rationale

Pointless whitespace.

#####How To Fix

Remove any whitespace from the end of the line.

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default true)