#!/usr/bin/env bash
# BASH fuzzyclock
# by Corey Mwamba
#
export TEXTDOMAINDIR="/usr/share/locale"
export TEXTDOMAIN=bash-fuzzy-clock

hr=($(date '+%_H'))
min=10#$(date '+%M')
nearly=$"nearly"
oclock=($"o'clock")

if [[ "${LANGUAGE:0:2}" != "" ]] ; then
    lng="${LANGUAGE:0:2}"
elif [[ "${LANG:0:2}" != "" ]] ; then
    lng="${LANG:0:2}"
fi

# To my knowledge, there is no natural way to say 
# "just after four", for example, in brazilian portuguese
case $lng in
    pt)
        justaft=""
    ;;
    *)
        justaft=$"just after"
    ;;
esac
if [[ $((min % 5)) -gt 0 ]];then
    if [[ $((min % 5)) -lt 3 ]]; then 
        adv=$justaft
    else
        adv=$nearly
    fi
fi

case "$1" in
    meri|m)
        if [[ $hr -gt 0 && $hr -lt 12 ]]; then
            echo $"morning"
        elif [[ $hr -ge 12 && $hr -lt 18 ]]; then
            echo $"afternoon"
        elif [[ $hr -ge 18 && $hr -lt 21 ]]; then
            echo $"evening"
        else
            echo $"night"
        fi
        ;;
    *)
        if [[ $min -gt 27 && $min -lt 33 ]]; then
            adj=$"half past"
            case "$lng" in
                de)
                    hr=$((hr + 1))
                    if [[ $hr -eq 24 ]]; then
                        hr=0
                    fi
            esac
        fi
        case "$lng" in
            pt)
                if [[ $min -ge 37 ]]; then 
                    hr=$((hr + 1))
                    if [[ $hr -eq 24 ]]; then
                        hr=0
                    fi
                fi
                ;;
            *)
                if [[ $min -ge 33 ]]; then
                    hr=$((hr + 1))

                    if [[ $hr -eq 24 ]]; then
                        hr=0
                    fi
                fi
        esac

        case $hr in
            1|13)
                th=$"one"
                ;;
            2|14)
                th=$"two"
                ;;
            3|15)
                th=$"three"
                ;;
            4|16)
                th=$"four"
                ;;
            5|17)
                th=$"five"
                ;;
            6|18)
                th=$"six"
                ;;
            7|19)
                th=$"seven"
                ;;
            8|20)
                th=$"eight"
                ;;
            9|21)
                th=$"nine"
                ;;
            10|22)
                th=$"ten"
                ;;
            11|23)
                th=$"eleven"
                ;;
            0)
                th=$"midnight"
                ;;
            12)
                th=$"midday"
        esac
        # French and Spanish use an hour declaration all the time
        # Italian does not
        # Most other languages treat one as a singular hour 
        #
        # But this might change if other languages are added.
        # I need to be able to control this better...
        case "$lng" in
            fr|es)
                if [[ $hr -eq 1 || $hr -eq 13 ]]; then
                    manner=$"hour"
                else
                    manner=$oclock
                fi
                ;;
            *)
                if [[ $hr -ne 12 && $hr -ne 0  ]]; then
                    if [[ $min -gt 57 || $min -lt 3 ]]; then
                        manner=$oclock
                    fi
                fi
        esac

        if [[ $min -ge 3 && $min -le 7 ]]; then
            adj=$"five past"
        fi

        if [[ $min -gt 7 && $min -lt 13 ]]; then
            adj=$"ten past"
        fi

        if [[ $min -ge 13 && $min -le 17 ]]; then
            adj=$"quarter past"
        fi

        if [[ $min -gt 17 && $min -lt 23 ]]; then
            adj=$"twenty past"
        fi

        if [[ $min -ge 23 && $min -le 27 ]]; then
            adj=$"twenty-five past"
        fi

        if [[ $min -ge 33 && $min -le 37 ]]; then
            adj=$"twenty-five to"

        fi

        if [[ $min -gt 37 && $min -lt 43 ]]; then
            adj=$"twenty to"
        fi

        if [[ $min -ge 43 && $min -le 47 ]]; then
            adj=$"quarter to"
        fi

        if [[ $min -gt 47 && $min -lt 53 ]]; then
            adj=$"ten to"
        fi

        if [[ $min -ge 53 && $min -le 57 ]]; then
            adj=$"five to"
        fi

        if [[ -z "$adv" && -z "$adj" ]]; then
            printf $"$th $manner\n"
        fi
        if [[ -z "$adv" && -n "$adj" ]]; then
            case $lng in
                pt)
                    if [[ $min -lt 37 ]]; then
                        printf $"$adj $th $manner\n"
                    else
                        printf $"$adj $th\n"
                    fi
                ;;
                *)
                printf $"$adj $th $manner\n"
                ;;
        esac
        fi
        if [[ -n "$adv" && -z "$adj" ]]; then
            printf $"$adv $th $manner\n"
        fi
        if [[ -n "$adv" && -n "$adj" ]]; then
            case $lng in
                pt)
                    if [[ $min -lt 37 ]]; then
                        printf $"$adv $adj $th $manner\n"
                    else
                        printf $"$adv $adj $th\n"
                    fi
                ;;
                *)
                    printf $"$adv $adj $th $manner\n"
                ;;
            esac
        fi
esac
