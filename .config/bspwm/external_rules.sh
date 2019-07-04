#! /bin/sh
# ~/.config/bspwm/scripts/external_rules.sh

# If a preselection exists, use it
has_presel=$(bspc query --nodes --node '.!automatic')

if [ ! -z $has_presel ]; then
        exit 0
fi

if [ -e /tmp/BSPWM-STACK-LOCK ]; then

    # Otherwise, it depends on the number of existing (non-float) windows
    num=$(bspc query -N -n '.leaf.!floating' -d | wc -l)
    
    if [ $num -eq 0 ]; then
        # No window, just put it here
        exit 0
    elif [ $num -eq 1 ]; then
        # One window, split east
        echo "split_dir=east"
        echo "split_ratio=0.5"
    else
        # More than one window, split south
        echo "node=@/2"
        echo "split_dir=south"
        echo "split_ratio=0.5"
    fi
fi
