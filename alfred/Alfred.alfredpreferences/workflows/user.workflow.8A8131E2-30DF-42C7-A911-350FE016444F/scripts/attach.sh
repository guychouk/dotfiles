#!/bin/bash
PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/bin:/bin:$PATH"
NAME="$1"
/Applications/kitty.app/Contents/MacOS/kitty --single-instance -- abduco -a "$NAME"
