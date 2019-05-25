if ! type scrot > /dev/null; then
    sudo dnf install -y scrot
fi

ICON=$HOME/.config/i3/lock-icon.png
TMPBG=/tmp/screen.png

scrot /tmp/screen.png

convert $TMPBG -scale 10% -scale 1000% $TMPBG
convert $TMPBG $ICON -gravity center -composite -matte $TMPBG

# pausa spotify
dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Stop

i3lock -u -i $TMPBG

rm $TMPBG
