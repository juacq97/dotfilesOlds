#!/bin/sh

password=$(find ~/.password-store/ -type f -name '*.gpg' |
	sed 's/.*\/\(.*\)\.gpg$/\1/' | dmenu -i -p "Pass:")
[ -n "$password" ] && pass show -c "$password"
