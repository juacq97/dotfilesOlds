#! /bin/sh

title () {
    var=$(playerctl --player=spotify metadata --format " {{title}}")
    check=$(echo $var | wc -m)
   
    if [[ $check -gt 80 ]]; then
	text=$(echo $var | cut -c1-30)
	echo $text...
    else
	echo $var
    fi
}

artist () {
    var=$(playerctl --player=spotify metadata --format " {{artist}}")
    check=$(echo $var | wc -m)
    
    if [[ $check -gt 25 ]]; then
	text=$(echo $var | cut -c1-25)
	echo $text...
    else
	echo $var
    fi
}

case $1 in
    "title") title;;
    "artist") artist;;
esac

