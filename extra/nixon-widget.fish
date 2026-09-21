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

# Puts what nixon ran into this shell's history, so the up-arrow finds it.
#
# `history append` arrived in fish 3.2; older fish keeps the log but adds
# nothing to its own history.
set -q NIXON_HISTORY_FILE
or set -g NIXON_HISTORY_FILE (test -n "$XDG_STATE_HOME"; and echo $XDG_STATE_HOME; or echo $HOME/.local/state)/nixon/history
function __nixon_history_size__
    test -f $NIXON_HISTORY_FILE
    or echo 0
    and stat -c %s $NIXON_HISTORY_FILE 2>/dev/null
    or stat -f %z $NIXON_HISTORY_FILE 2>/dev/null
    or echo 0
end

# Whatever is already logged counts as read, so sourcing this does not
# replay everything that ever ran.
set -g __nixon_history_seen (__nixon_history_size__)

function __nixon_history__ --on-event fish_prompt
    set -l size (__nixon_history_size__)
    test "$size" = "$__nixon_history_seen"; and return 0

    for entry in (tail -c +(math $__nixon_history_seen + 1) $NIXON_HISTORY_FILE)
        set -l line (string split -m 2 \t -- $entry)[3]
        test -n "$line"; and history append -- $line 2>/dev/null
    end
    set -g __nixon_history_seen $size
end

bind \ei nixon-insert-selection
bind \eI nixon-insert-command
bind \eP nixon-insert-project
bind \ep nixon-cd-project
