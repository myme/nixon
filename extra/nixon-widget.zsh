# nixon zle widgets.
#
# Alt-i  insert a selection from a command's output
# Alt-I  insert a command's source
# Alt-p  insert a project path
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

zle -N nixon-insert-selection
zle -N nixon-insert-command
zle -N nixon-insert-project

bindkey '\ei' nixon-insert-selection
bindkey '\eI' nixon-insert-command
bindkey '\ep' nixon-insert-project
