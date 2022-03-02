#!/usr/bin/env bash

herbstclient --idle "tag_*" 2>/dev/null | {

    while true; do
        # Read tags into $tags as array
        IFS=$'\t' read -ra tags <<< "$(herbstclient tag_status)"
        {
            for i in "${tags[@]}" ; do
                # Read the prefix from each tag and render them according to that prefix
                case ${i:1:2} in
		    '󰣇') #tag 1
			color=#FF5555
			echo "%{F$color}" ;;

		    '󰨇') #tag 2
			color=#50FA7B
			echo "%{F$color}";;

		    '󰹑') #tag 3
			color=#BD93F9
			echo "%{F$color}";;

		    '󰦊') #tag 4
			color=#FF79C6
			echo "%{F$color}";;
		    
		    '󱐀') #tag 5
			color=#8BE9FD
			echo "%{F$color}";;

		    '󰎄') #tag 6
			color=#1DB954
			echo "%{F$color}";;
		    
		esac


                case ${i:0:1} in
                    '#')
                        # the tag is viewed on the focused monitor
			echo "%{o$color}%{+o}"
                        # TODO Add your formatting tags for focused workspaces
                        ;;
                    ':')
                        # : the tag is not empty
                        # TODO Add your formatting tags for occupied workspaces

                        ;;
                    '!')
                        # ! the tag contains an urgent window
                        # TODO Add your formatting tags for workspaces with the urgent hint
                        ;;
                    '-')
			echo "%{o#828282}%{+o}"
                        # - the tag is viewed on a monitor that is not focused
                        # TODO Add your formatting tags for visible but not focused workspaces
                        ;;
			
                    *)
                        # . the tag is empty
                        # There are also other possible prefixes but they won't appear here
                        echo "%{F#64677B}" # Add your formatting tags for empty workspaces
                        ;;
                esac

                echo "%{A1:herbstclient use ${i:1}:}  ${i:1}  %{A -u -o F- B-}"
            done

            echo "%{F-}%{B-}"
        } | tr -d "\n"

    echo

    # wait for next event from herbstclient --idle
    read -r || break
done
} 2>/dev/null
