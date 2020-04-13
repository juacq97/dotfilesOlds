#!/bin/sh

# Gives a dmenu prompt to mount unmounted drives.
# If they're in /etc/fstab, they'll be mounted automatically.
# Otherwise, you'll be prompted to give a mountpoint from already existsing directories.
# If you input a novel directory, it will prompt you to create that directory.
pgrep -x dmenu && exit

getmount() { \
	[ -z "$chosen" ] && exit 1
	mp="$(find $1 | dmenu -i -p "Introduzca el punto de montaje.")"
	#mp="$(find $1 | rofi -theme drofi -dmenu -i -p "Introduzca el punto de montaje.")"
	[ "$mp" = "" ] && exit 1
	if [ ! -d "$mp" ]; then
		mkdiryn=$(printf "No\\nSi" | dmenu -i -p "$mp no existe. ¿Crearlo?")
		#mkdiryn=$(printf "No\\nSi" | rofi -theme drofi -dmenu -i -p "$mp no existe. ¿Crearlo?")
		[ "$mkdiryn" = "Si" ] && (mkdir -p "$mp" || sudo -A mkdir -p "$mp")
	fi
	}

mountusb() { \
	chosen="$(echo "$usbdrives" | dmenu -i -p "¿Qué dispositivo desea montar?" | awk '{print $1}')"
	#chosen="$(echo "$usbdrives" | rofi -theme drofi -dmenu -i -p "¿Qué dispositivo desea montar?" | awk '{print $1}')"
	udisksctl mount -b "$chosen" && notify-send -i "$PIX/usb.svg" "$chosen montado." && exit 0
	getmount "/mnt /media /mount /home -maxdepth 5 -type d"
	partitiontype="$(lsblk -no "fstype" "$chosen")"
	case "$partitiontype" in
		"vfat") sudo -A mount -t vfat "$chosen" "$mp" -o rw,umask=0000;;
		*) sudo -A mount "$chosen" "$mp"; user="$(whoami)"; ug="$(groups | awk '{print $1}')"; sudo -A chown "$user":"$ug" 741 "$mp";;
	esac
	notify-send -i "$PIX/usb.svg" "$chosen montado en $mp."
	}

mountandroid() { \
	chosen=$(echo "$anddrives" | dmenu -i -p "¿Qué dispositivo Android desea montar?" | cut -d : -f 1)
	#chosen=$(echo "$anddrives" | rofi -theme drofi -dmenu -i -p "¿Qué dispositivo Android desea montar?" | cut -d : -f 1)
	getmount "$HOME -maxdepth 3 -type d"
	simple-mtpfs --device "$chosen" "$mp"
	notify-send -i "$PIX/android.svg" "Dispositivo Android montado en $mp."
	}

asktype() { \
	case $(printf "USB\\nAndroid" | dmenu -i -p "¿Montar USB o dispositivo Android?") in
	#case $(printf "USB\\nAndroid" | rofi -theme drofi -dmenu -i -p "¿Montar USB o dispositivo Android?") in
		USB) mountusb ;;
		Android) mountandroid ;;
	esac
	}

anddrives=$(simple-mtpfs -l 2>/dev/null)
usbdrives="$(lsblk -rpo "name,type,size,mountpoint" | awk '$2=="part"&&$4==""{printf "%s (%s)\n",$1,$3}')"

if [ -z "$usbdrives" ]; then
	[ -z "$anddrives" ] && echo "Ningún dispositivo USB o Android detectado." && exit
	echo "Dispositivo Android detectado."
	mountandroid
else
	if [ -z "$anddrives" ]; then
		echo "Dispositivo USB detectado."
	       	mountusb
	else
		echo "Dispositivo USB y dispositivo Android montables detectados."
		asktype
	fi
fi
