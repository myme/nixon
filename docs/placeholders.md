# Placeholders

A placeholder makes one command a source of values for another. Nixon runs the
referenced command, turns its output into candidates, lets you pick from them,
and hands the result to the outer command.

Placeholders go either in the command heading, after the name, or in the code
fence's info string — not both; using both is an error.

````markdown
### `vim-file`

```bash ${git-files}
vim "$1"
```
````

Running `vim-file` first runs `git-files`, shows its output in the picker, and
passes what you pick to `vim` as `$1`.

## Grammar

```text
placeholder := start '{' name modifiers? '}'
start       := '<' | '$' | alias '='
name        := one or more characters other than " :|}"
```

`name` is the name of another command available in the same project. Hidden
`_commands` are the usual choice, since they exist only to produce candidates.

## How the value arrives

| Syntax | Effect |
|---|---|
| `${name}` | Each selected line becomes a positional argument: `$1`, `$2`, … |
| `<{name}` | The selection is piped to the command's stdin. |
| `={name}` | The selection, space-joined, is set as `$name` with `-` replaced by `_`. |
| `ALIAS={name}` | The same, under the name `ALIAS`. |

````markdown
### `vim-stdin`

```bash <{git-files | multi}
xargs vim -p
```

### `vim-env`

```bash ={git-files | multi}
vim -p $git_files
```

### `vim-env-alias`

```bash FILES={git-files | multi}
vim -p $FILES
```
````

## Modifiers

Modifiers are piped after the name, in any order.

With no format modifier, each output line is one candidate.

| Modifier | Effect |
|---|---|
| `\| fields N,M` | Split each line on whitespace and keep these fields, 1-based. |
| `\| cols N,M` | Split each line on the column positions the first row sets, and keep these columns. |
| `\| cols+h N,M` | The same, treating the first row as a header and dropping it. |
| `\| json` | Parse the whole output as a JSON array of candidates. |
| `\| multi` | Allow selecting several candidates. |
| `\| list` | Take every matching candidate without showing a picker. |
| `\| filter "text"` | Narrow the candidates with this query before selecting. |

The filter's query is an ordinary picker query, operators and all, so
`| filter "!test"` drops anything containing `test`. A filtered placeholder
waits for its command to finish rather than streaming, since the narrowing
happens before anything is shown.

Only one format modifier is allowed; a second is an error. Field and column
numbers are a comma-separated list of digits — `1,3`, not `1-3`. They are kept
in the line's own order regardless of the order given, and numbers past the end
of a line contribute nothing.

### Shorthand

`:` introduces a compact form of `fields` and `multi`, in either order:

| Shorthand | Long form |
|---|---|
| `${name:m}` | `${name \| multi}` |
| `${name:1}` | `${name \| fields 1}` |
| `${name:1,3m}` | `${name \| fields 1,3 \| multi}` |
| `${name:m1,3}` | `${name \| multi \| fields 1,3}` |

### JSON candidates

`| json` reads a JSON array whose elements are either a string, or an object
with `title` and `value`. The title is what the picker shows; the value is what
the command receives.

```json
[
  "src/main.rs",
  { "title": "the readme", "value": "README.md" }
]
```

## Streaming

Plain lines and `fields` produce candidates as the referenced command writes
them, so the picker opens immediately and fills as output arrives. `cols`,
`json`, `filter` and `list` wait for the command to finish, because column
widths, a JSON document and a narrowed list are only known once all of it is
there.

Cancelling the picker kills the referenced command's whole process group, not
just the interpreter.

## What the command receives

Besides the placeholder values, every command is given:

| Variable | Value |
|---|---|
| `nixon_project_path` | The project's directory. |
| `nixon_bin` | The nixon that started it, as an absolute path. |
| `nixon_opt_<name>` | `1` for each option that is on, empty for each that is off. |

`nixon_bin` is what a command uses to run another command; see
[calling nixon from a command](commands.md#calling-nixon-from-a-command).

## Arguments on the command line

Arguments after a command name are search queries for its placeholders, in
order, not values:

```shell
nixon vim-file main.rs
```

runs `git-files`, opens the picker with `main.rs` already typed, and — since
the picker auto-selects a unique match — runs `vim` on it without stopping if
exactly one file matches.

An argument equal to a candidate's **value** is taken as that candidate
without the picker appearing at all, even when it also fuzzy-matches others.
`nixon open bugs` picks `bugs` rather than asking between `bugs` and `bugs2`,
and for a `| multi` placeholder it selects just that one item. This is what
makes a placeholder command scriptable.

The comparison is literal, against the value the placeholder produces. Paths
are the case to watch: `nixon project -l` prints them with `~` for `$HOME`, so
a script that feeds them back should pass them in that form, or go through a
hidden `_command` that produces exactly what is wanted.

Arguments beyond the placeholders are passed through as positional arguments
unchanged.
