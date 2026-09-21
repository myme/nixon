# nixon fish widgets.
#
# Alt-h  run something nixon ran before
# Alt-H  insert it at the prompt instead
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

function nixon-insert-history
    commandline -i (nixon history -s)
    commandline -f repaint
end

# Runs what nixon ran before. Unlike zsh there is nowhere to park a
# half-typed line, so the buffer is replaced.
function nixon-run-history
    set -l line (nixon history -s)
    if test -n "$line"
        commandline -r -- $line
        commandline -f execute
    else
        commandline -f repaint
    end
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
    if not test -f $NIXON_HISTORY_FILE
        echo 0
        return
    end
    set -l size (stat -c %s $NIXON_HISTORY_FILE 2>/dev/null; or stat -f %z $NIXON_HISTORY_FILE 2>/dev/null)
    if test -n "$size"
        echo $size
    else
        echo 0
    end
end

# Whatever is already logged counts as read, so sourcing this does not
# replay everything that ever ran.
set -g __nixon_history_seen (__nixon_history_size__)

function __nixon_history__ --on-event fish_prompt
    set -l size (__nixon_history_size__)
    test "$size" = "$__nixon_history_seen"; and return 0

    # The log escapes `\`, tab and newline so one run is one line; awk puts
    # them back and separates the results with NUL, since an unescaped
    # invocation may itself span lines.
    set -l lines (tail -c +(math $__nixon_history_seen + 1) $NIXON_HISTORY_FILE \
        | awk -F'\t' '{ line = $3; out = ""; for (i = 1; i <= length(line); i++) { c = substr(line, i, 1); if (c == "\\") { i++; n = substr(line, i, 1); if (n == "t") out = out "\t"; else if (n == "n") out = out "\n"; else out = out n } else out = out c } if (out != "") printf "%s%c", out, 0 }' | string split0)
    for line in $lines
        test -n "$line"; and history append -- $line 2>/dev/null
    end
    set -g __nixon_history_seen $size
end

bind \eh nixon-run-history
bind \eH nixon-insert-history
bind \ei nixon-insert-selection
bind \eI nixon-insert-command
bind \eP nixon-insert-project
bind \ep nixon-cd-project
