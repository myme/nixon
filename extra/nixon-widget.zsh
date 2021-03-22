nixon-widget() {
    LBUFFER="${LBUFFER}$(nixon -b fzf -T run)"
    local ret=$?
    zle reset-prompt
    return $ret
}

zle -N nixon-widget
bindkey '\ei' nixon-widget
