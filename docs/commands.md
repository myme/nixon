# Commands

A command is a markdown heading whose text contains inline code, followed by a
fenced code block. The inline code is the command's name; the fence is what
runs.

````markdown
### `hello`

Says hello. This paragraph is the description.

```bash
echo "Hello, World!"
```
````

The first paragraph between the heading and the fence is the description, shown
beside the name in the picker. Anything else between them is skipped.

Inline code in a description keeps its own colour in the picker. Everything
nixon writes to stdout, `--list` included, is the plain text.

## Names

The name is the first word of the heading text. Everything after it is scanned
for [placeholders](placeholders.md).

- A name starting with `_` is **hidden**: it does not appear in the run picker,
  but placeholders can reference it, and `--list`, `edit` and `new` still see
  it. This is how a command exists only to produce candidates.
- A name ending in `&` runs **detached**: nixon starts it in its own process
  group and returns immediately, without waiting or propagating its status.

````markdown
### `terminal &`

```bash
x-terminal-emulator
```
````

## Options

A heading token of the form `-f` or `--name` declares an **option**: a flag
you toggle at the prompt, which lands in the command's arguments where the
heading put it.

````markdown
### `remove --force ${worktree}`

Removes a worktree.

- `--force`: on — also removes worktrees with local changes

```bash
git worktree remove "$@"
```
````

- The token is written as the command should receive it. `--depth=1` parses,
  but the value is not read yet; see the note below.
- The name is the token without its dashes and without any `=…`, so `--force`
  is `force` and `--no-cache` is `no-cache`.
- A list item of the form `` `--token`: on|off — description `` gives an
  option its default and the text shown beside it in the prompt. Without one,
  an option starts off.
- An option declared twice, a declaration for a token the heading does not
  declare, and a value other than `on` or `off` all fail the file.

At the prompt the options appear in a row under the command's name. `Alt-1` to
`Alt-9` flip them directly; see [the picker](picker.md) for the rest of the
keys. A command with options but no placeholders gets a small prompt of its
own — the toggles, `Enter` to run, `Esc` to cancel — and one with neither
prompts for nothing.

The prompt is a convenience, not a gate. With no terminal to draw on, a
command runs on its defaults rather than failing, so it stays usable from a
script.

On the command line, a word equal to an option's token turns it on and
`--no-<name>` turns it off:

```shell
nixon remove --force
nixon remove --no-force
```

Anything else stays a search query for the command's placeholders, so an
unknown `--x` searches rather than failing. A command line that settles every
option skips the prompt entirely.

Each option is also exported to the command as `nixon_opt_<name>`, `1` when
on and empty when off, with `-` replaced by `_`.

Only on/off options exist. The `=value` form is accepted by the grammar so
that valued and choice options can be added later without changing what is
already written.

## Languages

The code fence's info string names the interpreter.

| Info string | Runs with | Cached as |
|---|---|---|
| *(none)* | `$SHELL` | `.sh` |
| `sh`, `bash` | `bash` | `.sh` |
| `python` | `python3` | `.py` |
| `js`, `javascript` | `node` | `.js` |
| `haskell` | `runghc` | `.hs` |
| `json` | `jq -r .` | `.json` |
| `yaml` | `yq -r .` | `.yaml` |
| `plain` | `cat` | `.txt` |

Matching is case-sensitive, and any other info string is a language nixon does
not know: the command parses and lists, but running it fails with
`No interpreter for <lang>`.

## Scoping commands to project types

`type="…"` on a heading limits the commands beneath it to projects of that
type. It applies to every command nested under the heading, at any depth, until
a sibling or shallower heading.

````markdown
## Git stuff {type="git"}

### `git-files`

```bash
git ls-files
```
````

A heading may carry several `type=` attributes; any of them matching is enough.
A command with no type at all is available everywhere. Nested headings
accumulate, innermost first.

## Attributes

The heading attribute block is Pandoc's: `{.flag key="value"}`, after the
heading text.

| Attribute | Meaning |
|---|---|
| `.command` | Marks a heading as a command. Implied by inline code. |
| `.bg` | Runs detached. Implied by a name ending in `&`. |
| `.config` | The following code block is the config block. |
| `type="…"` | Scopes to a project type. Repeatable. |

Values are either bare (`[A-Za-z0-9_-]+`) or `"quoted"`, in which case anything
but a `"` goes. A heading whose attribute block does not parse is treated as
all name.

## What a command gets

- **`$nixon_project_path`** — the project's directory.
- **Working directory** — the project root.
- **Positional arguments** — from `${…}` placeholders, then anything left over
  on the command line.
- **Environment variables** — from `={…}` placeholders.
- **stdin** — from `<{…}` placeholders.

## Executables as commands

`bin_dirs` in the [config block](configuration.md) names directories, relative
to a project, whose executable files are offered as commands alongside the ones
written in markdown. The file's name is the command name; it is run directly.

## The script cache

Running a command writes its source to
`$XDG_CACHE_HOME/nixon/<sha1-of-source>-<name><ext>` and runs the interpreter on
that path. The script is never made executable and the interpreter is always
explicit, so a shebang line has no effect.

`nixon gc` empties the cache; `nixon gc --dry-run` prints what it would remove.
