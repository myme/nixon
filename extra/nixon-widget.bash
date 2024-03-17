nixon-widget() {
  local selected="$(nixon -b fzf -T run -s)"
  READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$selected${READLINE_LINE:$READLINE_POINT}"
  READLINE_POINT=$(( READLINE_POINT + ${#selected} ))
}

bind -x '"\ei": "nixon-widget"'

nixon-insert-project () 
{
    local project="$(nixon -b fzf -T project -s)"
    READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$project${READLINE_LINE:$READLINE_POINT}";
    READLINE_POINT=$(( READLINE_POINT + ${#project} ))
}

bind -x '"\ep": nixon-insert-project'
