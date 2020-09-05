#!/bin/sh

# xenocara wont automatically process ~/.profile.
# Therefore wont setup ENV for interactive shells.
. ~/.profile

export LANG=en_US.UTF-8

xrdb ~/.Xdefaults

if [ `uname -s` == "OpenBSD" ]; then
    xset +fp /usr/local/share/fonts/Liberation/  # Replace mscorettf
    xset +fp /usr/local/share/fonts/ghostscript/ # PDF/Print?
    xset +fp /usr/local/share/fonts/cantarell/   # Gnome fonts as of 3.0
    xset +fp /usr/local/share/fonts/terminus/    # Is this even in use?
    xset fp rehash
    xbanish &
    xdimmer &
    # xidle -timeout 600 -ne -program ~/bin/lock &
    # synclient ClickPad=1 HorizTwoFingerScroll=1 RightButtonAreaLeft=800 HorizHysteresis=10 VertHysteresis=10
fi

# Repeat rate
xset r rate 350 35

# Disable bell
xset b off

# BG color
xsetroot -solid '#6C7C87' -cursor_name left_ptr

# Mouse speed
# xset m 3/1 4
# xset was making too fast, and below was really just compensating.
# Remove both
# xinput set-prop /dev/wsmouse "Device Accel Constant Deceleration" 1.8

# This turns HHKB ctrl into caps
# This is probably better suited for a dock/undock.sh
# setxkbmap -option ctrl:nocaps
# xmodmap .Xmodmap

# Not using currently
# redshift -l 41.90:-87.66 -t 5500:3700 &

if [ -x /usr/local/bin/dbus-launch -a -z "${DBUS_SESSION_BUS_ADDRESS}" ]; then
	eval `dbus-launch --sh-syntax --exit-with-x11`
fi

#exec i3
exec cwm
