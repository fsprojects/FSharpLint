# Rule Suppression

Rules can be suppressed within the code using structured comments. Specify the type of suppression you want to use, followed by the rules to suppress. If there are no rules provided, all rules will be suppressed. Note that the available pool of rules is based on the loaded configuration file; a rule which is disabled in the configuration file cannot be re-enabled using one of these comments.

The following comments are available for use:

- `// fsharplint:disable` disables all rules for the rest of the file
- `// fsharplint:enable` enables all rules for the rest of the file
- `// fsharplint:disable TypePrefixing Hints` disables the specified rules for the rest of the file
- `// fsharplint:enable TypePrefixing Hints` enables the specified rules for the rest of the file
- `// fsharplint:disable-next-line` disables all rules for the next line of the file
- `// fsharplint:enable-next-line` enables all rules for the next line of the file
- `// fsharplint:disable-next-line TypePrefixing Hints` disables the specified rules for the next line of the file
- `// fsharplint:enable-next-line TypePrefixing Hints` enables the specified rules for the next line of the file
- `// fsharplint:disable-line` disables all rules for the current line of the file
- `// fsharplint:enable-line` enables all rules for the current line of the file
- `// fsharplint:disable-line TypePrefixing Hints` disables the specified rules for the current line of the file
- `// fsharplint:enable-next-line TypePrefixing Hints` enables the specified rules for the current line of the file

Note that only one structured comment can be specified per line; any additional ones will be ignored.

## Example

    // fsharplint:disable:next:line Hints
    let x = not true

This will not raise a warning, as the `Hints` rule is disabled for the next line.