#!/bin/sh

xrandr --output eDP-1 --auto --primary
xrandr --output DP-1 --off
setxkbmap -option ctrl:nocaps
