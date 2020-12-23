#! /bin/bash

# set prefered launcher
PREFERED_LAUNCHER=dmenu
# set path where urls will be stored
URL_FILE_PATH=$HOME/.bmks/
# name of file urls will be stored in
URL_FILE_NAME=urls

show_usage() {
	printf "bmks: unix bookmark management that sucks less

usage:
bmks help
	show this help message
bmks add <url>
	add a new bookmark
bmks del
	remove a bookmark
bmks ls
	show all bookmarks
bmks dmenu
	manual switch for displaying bookmarks in dmenu
bmks fzf
	manual switch for displaying bookmarks in fzf

Configuration is done by directly editing the script. Two launchers are available (dmenu and fzf). You can specify which one to use by adding to the PREFERED_LAUNCHER variable directly in the script. Both will display a menu that will allow you to choose a bookmark and open it in your default browser.

If you would prefer to have your bookmarks stored in alternate location there are also variables that can be changed for that. The default is /home/user/.bmks/urls\n"
}

bmks_add() {
	[ -z "$url" ] && printf "Error: url must be provided\n\n" && show_usage && exit 0
	printf "Description: "
	read description
	[ -z "$description" ] && echo "$url" >> $URL_FILE_PATH/$URL_FILE_NAME
	[ -n "$description" ] && echo "$description - $url" >> $URL_FILE_PATH/$URL_FILE_NAME
}

bmks_ls() {
	bmks_check
	cat $URL_FILE_PATH/$URL_FILE_NAME | sort
}

bmks_del() {
	bmks_check
	case $PREFERED_LAUNCHER in
		dmenu) sed -i "/$(cat $URL_FILE_PATH/$URL_FILE_NAME | sort | dmenu -l $(cat $URL_FILE_PATH/$URL_FILE_NAME | wc -l))/d" $URL_FILE_PATH/$URL_FILE_NAME ;;
		fzf) sed -i "/$(cat $URL_FILE_PATH/$URL_FILE_NAME | sort | fzf)/d" $URL_FILE_PATH/$URL_FILE_NAME ;;
	esac
}

bmks_display() {
	bmks_check
	case $PREFERED_LAUNCHER in
		dmenu) cat $URL_FILE_PATH/$URL_FILE_NAME | sort | dmenu -l $(cat $URL_FILE_PATH/$URL_FILE_NAME | wc -l) | awk '{print $(NF)}' | xargs -I '{}' $BROWSER {} ;;
		fzf) cat $URL_FILE_PATH/$URL_FILE_NAME | sort | fzf | awk '{print $(NF)}' | xargs -I '{}' $BROWSER {} ;;
	esac
}

bmks_check() {
	[ ! -s $URL_FILE_PATH/$URL_FILE_NAME ] && printf "Error: No bookmarks found to display. Try adding some!\n\n" && show_usage && exit 0
}

[ ! -d $URL_FILE_PATH ] && mkdir $URL_FILE_PATH
[ ! -f $URL_FILE_PATH/$URL_FILE_NAME ] && touch $URL_FILE_PATH/$URL_FILE_NAME

case "$1" in
	"help") show_usage ;;
	"add") url=$2; bmks_add ;;
	"del") bmks_del ;;
	"ls") bmks_ls ;;
	"dmenu") PREFERED_LAUNCHER=$1; bmks_display ;;
	"fzf") PREFERED_LAUNCHER=$1; bmks_display ;;
	*) bmks_display ;;
esac
