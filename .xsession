#!/bin/sh

# xenocara wont automatically process ~/.profile.
# Therefore wont setup ENV for interactive shells.
. ~/.profile

export LANG=en_US.UTF-8

xrdb ~/.Xdefaults

if [ `uname -s` == "OpenBSD" ]; then
    xset +fp /usr/local/share/fonts/mscorefonts/
    xset +fp /usr/local/share/fonts/ghostscript/
    xset +fp /usr/local/share/fonts/cantarell/
    xset +fp /usr/local/share/fonts/terminus/
    xset fp rehash
    xidle -timeout 600 -ne -program ~/bin/lock &
    xbanish &
    xdimmer &
    synclient ClickPad=1 HorizTwoFingerScroll=1 RightButtonAreaLeft=800 HorizHysteresis=10 VertHysteresis=10
fi

xset r rate 350 35
xset m 3/1 4
setxkbmap -option ctrl:nocaps
xmodmap .Xmodmap
redshift -l 41.90:-87.66 -t 5500:3700 &
xsetroot -solid '#6C7C87' -cursor_name left_ptr
exec i3
if [ -x /usr/local/bin/dbus-launch -a -z "${DBUS_SESSION_BUS_ADDRESS}" ]; then
	eval `dbus-launch --sh-syntax --exit-with-x11`
fi
