nixon-insert-selection() {
  local selected="$(nixon -b fzf -T run -s | while read -r item; do printf '%q ' "$item"; done)"
  READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$selected${READLINE_LINE:$READLINE_POINT}"
  READLINE_POINT=$(( READLINE_POINT + ${#selected} ))
}

bind -x '"\ei": "nixon-insert-selection"'

nixon-insert-command ()
{
    local command="$(nixon -b fzf -T run -i)"
    READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$command${READLINE_LINE:$READLINE_POINT}";
    READLINE_POINT=$(( READLINE_POINT + ${#command} ))
}

bind -x '"\eI": nixon-insert-command'

nixon-insert-project () 
{
    local project="$(nixon -b fzf -T project -s)"
    READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$project${READLINE_LINE:$READLINE_POINT}";
    READLINE_POINT=$(( READLINE_POINT + ${#project} ))
}

bind -x '"\ep": nixon-insert-project'
