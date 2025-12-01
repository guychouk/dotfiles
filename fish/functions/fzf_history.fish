function fzf_history
    # Use fzf to search fish history (clean output, no timestamps)
    set -l result (history | fzf --query=(commandline) --tiebreak=index)
    if test -n "$result"
        commandline -r -- $result
    end
    commandline -f repaint
end
