/home/gv/scripts/update-clock.sh

gpg-connect-agent updatestartuptty /bye

xrdb -merge ~/.Xresources
xsetroot -solid "#2e2e2e" &

# Disable screen blanking (DPMS)
xset s off
xset -dpms
xset s noblank

exec dwm
