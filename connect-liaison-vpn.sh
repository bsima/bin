#!/usr/bin/env sh

conn=$(nmcli con show --active|grep liaison)

export DISPLAY=:0

if [ -n "$conn" ]; then
    exit 0
else
    notify-send -t 3000 "Reconnecting to Liaison VPN..."
    nmcli con up liaison
    exit 0
fi
