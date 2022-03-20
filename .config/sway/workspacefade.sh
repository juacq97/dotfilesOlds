#!/bin/sh

name="$1"
# we need the workspace number for matching criteria.
number="${1%%:*}"

# we need the old workspace (the one we are on when we start) later for switching back to no-transparency
oldworkspace=$(swaymsg -t get_workspaces -p | grep "focused" | cut -d" " -f2)
oldworkspacenumber=${oldworkspace%%:*}

# Prevent flashing in and out if we target the current workspace
[ $name = $oldworkspace ] && exit

# Face out all windows in the current workspace
for i in `seq 20`; do
    swaymsg "[workspace=__focused__]" opacity minus 0.05
    sleep 0.0001
done

# make all the windows in target workspace invisible
swaymsg "[workspace=$number]" opacity 0
# and go there
swaymsg workspace "$name"

# With his line in place we can switch back the workspace to visible in case we move there differently (EG using swaybar)
swaymsg "[workspace=$oldworkspacenumber]" opacity 1

# Now fade in all the new windows
for i in `seq 20`; do
    swaymsg "[workspace=__focused__]" opacity plus 0.05
    sleep 0.0001
done
