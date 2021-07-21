#!/bin/sh

SAVE () {
    name=$(echo "" | dmenu -p "Enter layout name")
    echo "$name"
    if [[ $name == "" ]]; then exit 0; fi

    # Saving layout
    layout=$(herbstclient dump)
    echo "herbstclient load '$layout'" > ~/.config/herbstluftwm/layouts/$name

    # Saving windows configurations
    for id in $(herbstclient foreach C clients. echo C|grep -oE '0x[0-9a-fA-F]*') ; do
	client="clients.${id}"
	rule=(
            class="$(herbstclient get_attr ${client}.class)"
            instance="$(herbstclient get_attr ${client}.instance)"
            tag="$(herbstclient get_attr ${client}.tag)"
            title="$(herbstclient get_attr ${client}.title)"
	)
	if herbstclient compare "${client}.floating" = on ; then
            rule+=( "floating=on" )
            consequence=
	else
            rule+=(
		"index=$(herbstclient get_attr ${client}.parent_frame.index)"
            )
	fi
	echo herbstclient rule once "${rule[@]}" "# $id" >> ~/.config/herbstluftwm/layouts/$name
	echo herbstclient apply_tmp_rule -all "${rule[@]}" "# $id" >> ~/.config/herbstluftwm/layouts/$name
    done
}

LOAD () {
    sel=$(ls ~/.config/herbstluftwm/layouts | dmenu -p "Chose a layout" -i -l 10)
    if [[ $sel == "" ]]; then exit 0; fi
    cat ~/.config/herbstluftwm/layouts/$sel | sh
}

case $1 in
    "save") SAVE;;
    "load") LOAD;;
    *) echo Error;;
esac
