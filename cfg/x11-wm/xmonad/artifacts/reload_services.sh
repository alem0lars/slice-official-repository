#!/bin/sh


# Trayer -> Tray panel
killall trayer-srg
trayer-srg --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 30 --widthtype percent --alpha 0 --transparent true --tint 0x1a1a1a1a --height 20 --heighttype pixel &

# Feh -> Set the background image
killall feh
feh --bg-tile ~/.xmonad/resources/images/background.png &

# Conky -> System monitor
killall conky
conky -c /etc/conky/conky_1.conf -d
conky -c /etc/conky/conky_2.conf -d
conky -c /etc/conky/conky_3.conf -d

