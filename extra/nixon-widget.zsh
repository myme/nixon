nixon-widget() {
    LBUFFER="${LBUFFER}$(nixon -b fzf -T run -s)"
    local ret=$?
    zle reset-prompt
    return $ret
}

zle -N nixon-widget
bindkey '\ei' nixon-widget
