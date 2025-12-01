# Set kitty tab title
function set_kitty_tab_title
    # only set title if we're in kitty
    if test "$TERM" = "xterm-kitty"
        set title $argv[1]
        # If no title was passed, use the current directory name
        if test -z "$title"
            set title (basename $PWD)
            if test "$PWD" = "$HOME"
                set title "~"
            end
        end
        # use kitty remote control to set tab title
        kitten @ set-tab-title "$title" 2>/dev/null
    end
end
