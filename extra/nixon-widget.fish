# nixon fish widgets.
#
# Alt-i  insert a selection from a command's output
# Alt-I  insert a command's source
# Alt-p  cd into a project
# Alt-P  insert a project path
#
# Source this from ~/.config/fish/config.fish.

function nixon-insert-selection
    # Escaped per item: a selection may hold spaces or shell characters.
    commandline -i (nixon run -s | string escape | string join ' ')
    commandline -f repaint
end

function nixon-insert-command
    commandline -i (nixon run -i)
    commandline -f repaint
end

function nixon-insert-project
    commandline -i (nixon project -s | string escape)
    commandline -f repaint
end

# fish changes directory in the function itself; nothing is typed, so there
# is no history entry either way.
function nixon-cd-project
    set -l dir (nixon project -s | head -n 1)
    if test -n "$dir"
        cd -- $dir
    end
    commandline -f repaint
end

bind \ei nixon-insert-selection
bind \eI nixon-insert-command
bind \eP nixon-insert-project
bind \ep nixon-cd-project
