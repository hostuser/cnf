#!/bin/bash

xkbcomp -I$HOME/config/xkb $HOME/config/xkb/keymap/filco $DISPLAY 2> /tmp/display
echo $DISPLAY >> /tmp/display
echo done >> /tmp/display



