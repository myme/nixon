# The picker

Nixon has its own fuzzy picker; there is no `fzf` or `rofi` to install. It
draws on **stderr**, so stdout stays clean for whatever the selection feeds.

The layout follows fzf's: a query line at the top with the match counts on the
right, a header under it naming what is being selected, and the candidates
below. The current row has a pointer in the margin and a subdued background;
the characters your query matched are picked out in the row text.

The counts read `matched/total`, with `(marked)` added when several may be
selected.

## Choosing

| Key | Action |
|---|---|
| `Enter` | Confirm the selection |
| `Alt-Enter` | Edit the command's source before running it |
| `F1` | Print the command's source instead of running it |
| `F2` | Open the command where it is defined, in `$EDITOR` |
| `Tab` | Mark the row and move down |
| `Shift-Tab` | Mark the row and move up |
| `Esc`, `Ctrl-C`, `Ctrl-G` | Cancel; nixon exits 130 |

Marks stick. Search for one thing and mark a few rows, then search for
something else and mark more: `Enter` returns all of them, including the rows
the current query no longer shows.

## Options

A command that declares [options](commands.md#options) shows them in a row
under its name, above the query: `[x] --force  [ ] -v`.

Wherever the focus is:

| Key | Action |
|---|---|
| `Alt-1` … `Alt-9` | Flip the first to ninth option |
| `Alt-o` | Move the focus onto the row, or back to the query |

With the focus **on the row**:

| Key | Action |
|---|---|
| `Space` | Flip the focused option |
| `Left`, `Shift-Tab` | Move left; off the first option, back to the query |
| `Right`, `Tab` | Move right; past the last option, back to the query |

Every other key means on the row what it means anywhere else: `Enter` runs the
selection, `Esc` cancels the pick, `Ctrl-N` and `Ctrl-P` move in the list.
Typing is the exception — it goes nowhere while the row has focus, so a stray
letter cannot quietly change the query.

The toggles carry from one placeholder prompt to the next, so a command with
several placeholders is only asked about them once.

A command with options and no placeholders gets a prompt with the row and
nothing else, where `Space` toggles, `Enter` runs and `Esc` cancels.

## Moving

| Key | Action |
|---|---|
| `Down`, `Ctrl-N`, `Ctrl-J` | Next candidate |
| `Up`, `Ctrl-P`, `Ctrl-K` | Previous candidate |
| `PgDn`, `Ctrl-V` | Down a page |
| `PgUp`, `Alt-V` | Up a page |
| `Alt-J`, `Alt-K` | Down, up half a page |

`Ctrl-J` and `Ctrl-K` move in the list rather than editing the line, as in fzf.

## Editing the query

The query line follows readline. The same keys work in the editor that
`Alt-Enter` opens, where `Up` and `Down` move between lines and `Alt-Enter`
inserts a newline.

| Key | Action |
|---|---|
| `Ctrl-A`, `Home` | Start of line |
| `Ctrl-E`, `End` | End of line |
| `Ctrl-B`, `Left` | Back one character |
| `Ctrl-F`, `Right` | Forward one character |
| `Alt-B`, `Alt-Left` | Back one word |
| `Alt-F`, `Alt-Right` | Forward one word |
| `Backspace`, `Ctrl-H` | Delete before the cursor |
| `Delete`, `Ctrl-D` | Delete under the cursor |
| `Ctrl-W`, `Alt-Backspace` | Delete the word before the cursor |
| `Alt-D` | Delete the word after the cursor |
| `Ctrl-U` | Delete to the start of the line |
| `Ctrl-Y` | Paste back what was last deleted |

## Matching

Matching is fuzzy by default, and ranks the way fzf does: by score, then by the
length of the text matched, then by the order candidates arrived.

A query is a set of terms separated by spaces, and a candidate has to satisfy
all of them. Each term may carry one of fzf's operators:

| Term | Matches |
|---|---|
| `foo` | Fuzzily: the characters in order, anywhere. |
| `'foo` | The literal substring `foo`. |
| `^foo` | Candidates starting with `foo`. |
| `foo$` | Candidates ending with `foo`. |
| `!foo` | Candidates **not** containing `foo`. |

`!` combines with the others: `!^foo` and `!foo$` both work. A space, or an
operator meant literally, is escaped with a backslash: `foo\ bar`, `\^foo`.

`a | b` for alternatives is the one part of fzf's syntax that is missing; the
matcher does not implement it.

By default, case is handled the way fzf handles it: a query in lower case
ignores case, and a query with a capital in it does not.

- `exact_match` (or `-e`) makes a plain term a substring match rather than a
  fuzzy one. `'foo` then means the opposite — match it fuzzily.
- `ignore_case` (or `-i`) ignores case whatever the query looks like, and
  `--no-ignore-case` compares case exactly.

Comparing case exactly has a sharp edge: a `^` or `$` term then matches
nothing, because the matcher compares the whole candidate rather than the
anchored part. `'` terms are unaffected. Smart case and `-i` are both fine.

Candidates may carry colour; matching always runs on the visible text, so a
query never matches an escape sequence and the highlights line up with what you
see.

## Opening and auto-selecting

A query that matches exactly **one** candidate selects it without the picker
ever appearing. This is decided once, on the query the picker opens with —
narrowing to a single row by typing does not select it for you.

Because of that, the terminal is claimed lazily: nothing is drawn while a
unique match is still possible. A command producing thousands of candidates
opens the picker at once; `nixon some-unique-command` never draws at all, which
is what makes it safe in a script with no terminal.

Candidates stream in where the format allows it, so the picker opens
immediately and fills as they arrive. Cancelling kills the producing command's
whole process group.
