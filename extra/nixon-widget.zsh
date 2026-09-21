# nixon zle widgets.
#
# Alt-i  insert a selection from a command's output
# Alt-I  insert a command's source
# Alt-p  cd into a project
# Alt-P  insert a project path
#
# Source this from ~/.zshrc.

nixon-insert-selection() {
  LBUFFER="${LBUFFER}$(nixon run -s)"
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
  LBUFFER="${LBUFFER}$(nixon project -s)"
  local ret=$?
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

zle -N nixon-insert-selection
zle -N nixon-insert-command
zle -N nixon-insert-project
zle -N nixon-cd-project

bindkey '\ei' nixon-insert-selection
bindkey '\eI' nixon-insert-command
bindkey '\eP' nixon-insert-project
bindkey '\ep' nixon-cd-project
