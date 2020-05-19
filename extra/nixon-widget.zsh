nixon-widget() {
    LBUFFER="${LBUFFER}$(nixon -b fzf run --select)"
    local ret=$?
    zle reset-prompt
    return $ret
}

zle -N nixon-widget
bindkey '\ei' nixon-widget
