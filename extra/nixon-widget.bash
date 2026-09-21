# nixon readline widgets.
#
# Alt-i  insert a selection from a command's output
# Alt-I  insert a command's source
# Alt-p  cd into a project
# Alt-P  insert a project path
#
# Source this from ~/.bashrc.

nixon-insert-selection() {
  local selected
  selected="$(nixon run -s | while read -r item; do printf '%q ' "$item"; done)"
  READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$selected${READLINE_LINE:$READLINE_POINT}"
  READLINE_POINT=$((READLINE_POINT + ${#selected}))
}

nixon-insert-command() {
  local command
  command="$(nixon run -i)"
  READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$command${READLINE_LINE:$READLINE_POINT}"
  READLINE_POINT=$((READLINE_POINT + ${#command}))
}

nixon-insert-project() {
  local project
  project="$(nixon project -s)"
  READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$project${READLINE_LINE:$READLINE_POINT}"
  READLINE_POINT=$((READLINE_POINT + ${#project}))
}

# Prints the `cd` for the chosen project, or nothing if none was chosen.
#
# `bind -x` cannot do this: a function that changes directory leaves the
# prompt showing the old one. The macro below types the command instead, so
# readline runs it and redraws, which is how fzf's Alt-C works.
__nixon_cd__() {
  local dir
  dir="$(nixon project -s | head -n 1)" &&
    [[ -n $dir ]] &&
    printf 'builtin cd -- %q' "$(builtin unset CDPATH && builtin cd -- "$dir" && builtin pwd)"
}

# Puts what nixon ran into this shell's history, so Ctrl-R finds it.
#
# Cheap when nothing has happened: one `wc -c` against a remembered byte
# count, and no work at all until the log exists.
NIXON_HISTORY_FILE="${NIXON_HISTORY_FILE:-${XDG_STATE_HOME:-$HOME/.local/state}/nixon/history}"

__nixon_history_size__() {
  local size=0
  [[ -f $NIXON_HISTORY_FILE ]] && size=$(wc -c <"$NIXON_HISTORY_FILE")
  printf '%s' "${size// /}"
}

# Whatever is already logged counts as read, so sourcing this does not
# replay everything that ever ran. Taken now rather than at the first
# prompt, or the first command of the session would be swallowed with it.
__nixon_history_seen=$(__nixon_history_size__)

__nixon_history__() {
  local size
  size=$(__nixon_history_size__)
  [[ $size == "$__nixon_history_seen" ]] && return 0

  local line
  while IFS=$'\t' read -r _ _ line; do
    [[ -n $line ]] && builtin history -s "$line"
  done < <(tail -c "+$((__nixon_history_seen + 1))" "$NIXON_HISTORY_FILE")
  __nixon_history_seen=$size
}

case "${PROMPT_COMMAND-}" in
  *__nixon_history__*) ;;
  "") PROMPT_COMMAND="__nixon_history__" ;;
  *) PROMPT_COMMAND="${PROMPT_COMMAND%;};__nixon_history__" ;;
esac

bind -x '"\ei": nixon-insert-selection'
bind -x '"\eI": nixon-insert-command'
bind -x '"\eP": nixon-insert-project'

# fzf's Alt-C macro, less one sequence. `\C-k`/`\C-u` save whatever was
# already typed and the tail after `\C-m` puts it back.
#
# The `cd` does land in history here, as it does with fzf. The macro's
# leading space cannot prevent that: it is killed by the `\C-b\C-k` that
# saves the old line, and a space printed by __nixon_cd__ instead does not
# survive either, because `\e\C-e` expands the line as words and drops
# leading whitespace. zsh and fish do keep it out; see the docs.
#
# fzf has `\C-\e(` between the expansion and `\C-m`. In bash 5.3 that is an
# unbound sequence, and readline abandons the rest of a macro when it hits
# one: the `cd` is typed out and never run. Dropping it is what makes this
# work, and the line is otherwise fzf's.
#
# shellcheck disable=SC2016  # the macro is literal readline input, not shell
bind -m emacs-standard '"\ep": " \C-b\C-k \C-u`__nixon_cd__`\e\C-e\C-m\C-y\C-h\e \C-y\ey\C-x\C-x\C-d\C-y\ey\C-_"'
bind -m vi-command '"\C-z": emacs-editing-mode'
bind -m vi-insert '"\C-z": emacs-editing-mode'
bind -m emacs-standard '"\C-z": vi-editing-mode'
bind -m vi-command '"\ep": "\C-z\ep\C-z"'
bind -m vi-insert '"\ep": "\C-z\ep\C-z"'
