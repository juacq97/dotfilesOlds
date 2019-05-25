#!/bin/sh
### Created by ilsenatorov
### Change WALLPAPERDIR to the directory where you store wallpapers

WALLPAPERDIR=~/Drive/WALLPAPERS/

if [ -z $@ ]
then
function get_themes()
{
    ls $WALLPAPERDIR
}
echo current; get_themes
else
    THEMES=$@
    if [ x"current" = x"${THEMES}" ]
    then
	exit 0
        # wal -i `cat ~/.cache/wal/wal` > /dev/null #Allows you to just stay with current theme
    elif [ -n "${THEMES}" ]
    then
        wal -i $WALLPAPERDIR${THEMES} > /dev/null
    fi
fi
