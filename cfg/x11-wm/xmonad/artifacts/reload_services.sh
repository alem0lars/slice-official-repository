#!/bin/sh


# Trayer -> Tray panel
echo ">> Killing trayer"
killall trayer-srg > /dev/null
echo ">> Starting trayer"
nohup trayer-srg --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 20 --widthtype percent --alpha 0 --transparent true --tint 0x1a1a1a1a --height 20 --heighttype pixel > /dev/null &


# Feh -> Set the background image
echo ">> Background setup"
feh --bg-scale ~/.xmonad/resources/images/background.png


# Conky -> System monitor
echo ">> Killing conky"
killall conky > /dev/null
echo ">> Starting conky"
conky -c /etc/conky/conky_1.conf -d &> /dev/null
conky -c /etc/conky/conky_2.conf -d &> /dev/null
conky -c /etc/conky/conky_3.conf -d &> /dev/null

# XMonad
xmonad --restart

# XSetRoot -> Setup cursor style
xsetroot -cursor_name left_ptr

