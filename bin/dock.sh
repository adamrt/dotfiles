#!/bin/sh

xrandr --output DP-1 --auto --primary
xrandr --output eDP-1 --off
# Unset ctrl:nocaps with empty options
setxkbmap -option
