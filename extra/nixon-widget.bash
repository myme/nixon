# nixon readline widgets.
#
# Alt-i  insert a selection from a command's output
# Alt-I  insert a command's source
# Alt-p  insert a project path
#
# Source this from ~/.bashrc.

nixon-insert-selection() {
  local selected
  selected="$(nixon run -s | while read -r item; do printf '%q ' "$item"; done)"
  READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$selected${READLINE_LINE:$READLINE_POINT}"
  READLINE_POINT=$(( READLINE_POINT + ${#selected} ))
}

nixon-insert-command() {
  local command
  command="$(nixon run -i)"
  READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$command${READLINE_LINE:$READLINE_POINT}"
  READLINE_POINT=$(( READLINE_POINT + ${#command} ))
}

nixon-insert-project() {
  local project
  project="$(nixon project -s)"
  READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$project${READLINE_LINE:$READLINE_POINT}"
  READLINE_POINT=$(( READLINE_POINT + ${#project} ))
}

bind -x '"\ei": nixon-insert-selection'
bind -x '"\eI": nixon-insert-command'
bind -x '"\ep": nixon-insert-project'
