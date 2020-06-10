---
title: Rule Suppression
category: how-to
menu_order: 4
---

# Rule Suppression

The linter's rules can be suppressed using structured comments.

The following comments are available for use:

- `// fsharplint:disable` disables rules for all lines following the comment.
- `// fsharplint:enable` re-enables all disabled rules.
- `// fsharplint:disable-next-line` disables rules for the next line.
- `// fsharplint:disable-line` disables rules for the current line.

Only one structured comment can be specified per line; any additional ones will be ignored.

## Disabling Specific Rules

The disable comments allow for specific rules to be disabled by specifying the rules whitespace separated at the end of the comment, for example:

- `// fsharplint:disable TypePrefixing Hints` disables the rules `TypePrefixing` and `Hints`.
- `// fsharplint:disable-next-line TypePrefixing` disables the rule `TypePrefixing`.

If no rules a specified then all rules are disabled, for example:

- `// fsharplint:disable` disables all rules.

## Examples

### Disable Entire File

```fsharp
// fsharplint:disable
```

Precede the file with the comment above.

### Disable Section of the File

```fsharp
// fsharplint:disable RecordFieldNames
type MyType =
    { xyz: string
      foo: string }
// fsharplint:enable
```

Disables the `RecordFieldNames` rule between the disable and enable comments.

```fsharp
// fsharplint:disable
type sometype =
    { xyz: string
      foo: string }
// fsharplint:enable
```

Disables all rules between the disable and enable comments.

### Disable Next Line

```fsharp
// fsharplint:disable-next-line Hints
let x = not true
```

Disables the `Hints` rule for the next line.

```fsharp
// fsharplint:disable-next-line
let x = not true
```

Disables all rules for the next line.

### Disable Current Line

```fsharp
let x = not true // fsharplint:disable-line Hints
```

Disables the `Hints` rule for the current line.

```fsharp
let x = not true // fsharplint:disable-line
```

Disables all rules for the current line.
