#!/bin/sh
#
# Author: jzbor
# Dependencies: xdo, xwininfo (optional), xdg-xmenu (optional), xmenu (optional
#
# Usage:
#   riot.sh
#   riot.sh xterm
#   riot.sh --focused
#   riot.sh --select


dims="$(slop -t 0 -l -c 0.92,0.85,0.69,0.3 -f "%x+%y+%w+%h")"
[ -n "$dims" ] || exit 1

if [ "$1" = "--select" ]; then
    if command -v xwininfo > /dev/null 2>&1; then
        wid="$(xwininfo | grep "Window id:" | cut -d' ' -f4)"
        selector="$wid"
    else
        echo "You need xwininfo for this option to work"
        exit 1
    fi
elif [ "$1" = "--focused" ]; then
    wid="$(xdo id)"
    selector="$wid"
elif [ -z "$1" ]; then
    if command -v xdg-xmenu > /dev/null 2>&1 \
            && command -v xmenu > /dev/null 2>&1; then
        run="$(xdg-xmenu | xmenu)"
    else
        echo "You need xdg-xmenu and xmenu for this option to work"
        exit 1
    fi
else
    run="$1"
fi

if [ "$1" != "--select" ] && [ "$1" != "--focused" ]; then
    if ! (command -v "$(echo $run | cut -d' ' -f1)" > /dev/null 2>&1); then
        echo "$run not runnable"
        exit 1
    else
        $run &
        pid="$!"
    fi
fi

x="$(echo $dims | cut -d"+" -f1)"
y="$(echo $dims | cut -d"+" -f2)"
width="$(echo $dims | cut -d"+" -f3)"
height="$(echo $dims | cut -d"+" -f4)"

if [ -z "$wid" ]; then
    i=0
    while ! (xdo id -p $pid) && [ "$i" -lt 50 ]; do
        echo $i : $(xdo id -p $pid) $pid
        sleep 0.1
        i=$((i+1))
    done
    sleep 0.2

    wid="$(xdo id -p $pid | tail -n 1)"
    echo ids: $wid $pid
    [ -n "$wid" ] || exit
    selector="-p $pid"
fi

xdo resize -w $width -h $height $selector
sleep 0.05
xdo move -x $x -y $y $selector
