/home/gv/scripts/update-clock.sh
gpg-connect-agent updatestartuptty /bye
xrdb -merge ~/.Xresources
xsetroot -solid "#2e2e2e" &
exec dwm
