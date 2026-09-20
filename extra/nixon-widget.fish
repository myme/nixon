# nixon fish widgets.
#
# Alt-i  insert a selection from a command's output
# Alt-I  insert a command's source
# Alt-p  insert a project path
#
# Source this from ~/.config/fish/config.fish.

function nixon-insert-selection
    commandline -i (nixon run -s | string join ' ')
    commandline -f repaint
end

function nixon-insert-command
    commandline -i (nixon run -i)
    commandline -f repaint
end

function nixon-insert-project
    commandline -i (nixon project -s)
    commandline -f repaint
end

bind \ei nixon-insert-selection
bind \eI nixon-insert-command
bind \ep nixon-insert-project
