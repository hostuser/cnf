#!/bin/bash

scrot /tmp/screen_locked.png
convert /tmp/screen_locked.png -scale 2% -scale 5000% /tmp/screen_locked2.png
i3lock -i /tmp/screen_locked2.png
