function set_kitty_tab_title --description "Sets kitty's tab title (no arguments uses pwd as title)"
    # only set title if we're in kitty
    if test "$TERM" = "xterm-kitty"
        set title $argv[1]
        # if no title was passed, use the current directory name
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
