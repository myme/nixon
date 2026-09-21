# nixon zle widgets.
#
# Alt-h  insert something nixon ran before
# Alt-i  insert a selection from a command's output
# Alt-I  insert a command's source
# Alt-p  cd into a project
# Alt-P  insert a project path
#
# Source this from ~/.zshrc.

nixon-insert-selection() {
  # Quoted per value: a selection may hold spaces or shell characters.
  # `(f)` unquoted splits on newlines and drops the empty ones.
  local -a picked
  picked=(${(f)"$(nixon run -s)"})
  local ret=$?
  (($#picked)) && LBUFFER="${LBUFFER}${(j: :)${(@q)picked}} "
  zle reset-prompt
  return $ret
}

nixon-insert-history() {
  LBUFFER="${LBUFFER}$(nixon history -s)"
  local ret=$?
  zle reset-prompt
  return $ret
}

nixon-insert-command() {
  LBUFFER="${LBUFFER}$(nixon run -i)"
  local ret=$?
  zle reset-prompt
  return $ret
}

nixon-insert-project() {
  # Quoted: a project path may hold spaces.
  local project
  project="$(nixon project -s)"
  local ret=$?
  [[ -n $project ]] && LBUFFER="${LBUFFER}${(q)project}"

  zle reset-prompt
  return $ret
}

# Changes directory by running the `cd` as if it had been typed, which is
# what redraws the prompt. The leading space keeps it out of history for
# anyone with HIST_IGNORE_SPACE; without that option the `cd` is recorded,
# as it is with fzf.
nixon-cd-project() {
  setopt localoptions pipefail no_aliases 2>/dev/null
  local dir
  dir="$(nixon project -s </dev/tty | head -n 1)"
  if [[ -z "$dir" ]]; then
    zle redisplay
    return 0
  fi
  # The absolute path, so the history entry works from anywhere. `:a` would
  # resolve symlinks, which `cd` does not.
  dir=$(builtin cd -q >/dev/null -- "${dir}" && echo "${PWD}" || echo "${dir}")
  zle push-line # Clear the buffer; zsh restores it at the next prompt.
  BUFFER=" builtin cd -- ${(q)dir}"
  zle accept-line
  local ret=$?
  unset dir
  zle reset-prompt
  return $ret
}

# Puts what nixon ran into this shell's history, so Ctrl-R finds it.
#
# Cheap when nothing has happened: one stat against a remembered byte count,
# and no work at all until the log exists.
: "${NIXON_HISTORY_FILE:=${XDG_STATE_HOME:-$HOME/.local/state}/nixon/history}"
zmodload -F zsh/stat b:zstat 2>/dev/null

# Into a variable rather than through a subshell, so a prompt with nothing
# to do costs no fork at all.
__nixon_history_size__() {
  local -a stat
  __nixon_history_size=0
  [[ -f $NIXON_HISTORY_FILE ]] || return 0
  zstat -A stat +size -- "$NIXON_HISTORY_FILE" 2>/dev/null || return 0
  __nixon_history_size=$stat[1]
}

# Whatever is already logged counts as read, so sourcing this does not
# replay everything that ever ran. Taken now rather than at the first
# prompt, or the first command of the session would be swallowed with it.
typeset -g __nixon_history_size=0
__nixon_history_size__
typeset -g __nixon_history_seen=$__nixon_history_size

__nixon_history__() {
  __nixon_history_size__
  local size=$__nixon_history_size
  [[ $size == $__nixon_history_seen ]] && return 0

  local line
  while IFS=$'\t' read -r _ _ line; do
    [[ -n $line ]] && print -s -- "$line"
  done < <(tail -c "+$((__nixon_history_seen + 1))" "$NIXON_HISTORY_FILE")
  __nixon_history_seen=$size
}

typeset -ga precmd_functions
(($precmd_functions[(I)__nixon_history__])) || precmd_functions+=(__nixon_history__)

zle -N nixon-insert-history
zle -N nixon-insert-selection
zle -N nixon-insert-command
zle -N nixon-insert-project
zle -N nixon-cd-project

bindkey '\eh' nixon-insert-history
bindkey '\ei' nixon-insert-selection
bindkey '\eI' nixon-insert-command
bindkey '\eP' nixon-insert-project
bindkey '\ep' nixon-cd-project
