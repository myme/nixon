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
