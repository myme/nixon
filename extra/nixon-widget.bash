nixon-widget() {
  local selected="$(nixon -b fzf -T run -s)"
  READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$selected${READLINE_LINE:$READLINE_POINT}"
  READLINE_POINT=$(( READLINE_POINT + ${#selected} ))
}

bind -x '"\ei": "nixon-widget"'
