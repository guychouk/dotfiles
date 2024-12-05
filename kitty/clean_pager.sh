#!/bin/zsh

# Clean ANSI (color) and OSC sequences, then open in Vim
sed -r '
    # Remove ANSI CSI sequences (e.g., \x1B[...m, including colors like \x1B[38;5;...m)
    s/\x1B\[[0-9;]*[a-zA-Z]//g;
    # Remove OSC sequences (\x1B]... terminated by \x1B\ or \x07)
    s/\x1B\][^\x07\x1B]*(\x07|\x1B\\)//g;
    # Remove sequences with explicit \x1B escape characters (non-CSI, non-OSC)
    s/\x1B\[[^m]*m//g;
    # Handle any remaining \x1B[... patterns
    s/\x1B\[[^a-zA-Z]*//g
' | vim -c "set nonumber nolist showtabline=0 foldcolumn=0 ft=bash laststatus=0" -c "let @/='\a'" -c "norm! GN" -c "nmap Q :q!<CR>" -
