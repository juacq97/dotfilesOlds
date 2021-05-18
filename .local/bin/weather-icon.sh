#!/bin/sh

get_icon() {
    case $1 in
        # Icons for weather-icons
        01d) icon="☀";;
        01n) icon="🌙";;
        02d) icon="🌤";;
        02n) icon="🌤";;
        03*) icon="☁";;
        04*) icon="☁";;
        09d) icon="🌧";;
        10d) icon="🌧";;
        10n) icon="🌦";;
        11d) icon="🌩";;
        11n) icon="🌩";;
        13d) icon="🌨";;
        13n) icon="🌨";;
        50d) icon="🌫";;
        50n) icon="🌫";;
        *) icon="☀";


        # Icons for Font Awesome 5 Pro
  #      01d) icon="";;
  #      01n) icon="";;
  #      02d) icon="";;
  #      02n) icon="";;
  #      03d) icon="";;
  #      03n) icon="";;
  #      04*) icon="";;
  #      09*) icon="";;
  #      10d) icon="";;
  #      10n) icon="";;
  #      11*) icon="";;
  #      13*) icon="";;
  #      50*) icon="";;
  #      *) icon="";

	   # Icons for material design icons
        #01d) icon="";;
        #01n) icon="";;
        #02d) icon="";;
        #02n) icon="";;
        #03d) icon="";;
        #03n) icon="";;
        #04*) icon="";;
        #09*) icon="";;
        #10d) icon="";;
        #10n) icon="";;
        #11*) icon="";;
        #13*) icon="";;
        #50*) icon="";;
        #   *) icon="";
    esac

    echo $icon
}

KEY="94d0cc92f88971efe48469ddabe2f6ce"
CITY="4000900"
UNITS="metric"
SYMBOL="°C"

API="https://api.openweathermap.org/data/2.5"

if [ -n "$CITY" ]; then
    if [ "$CITY" -eq "$CITY" ] 2>/dev/null; then
        CITY_PARAM="id=$CITY"
    else
        CITY_PARAM="q=$CITY"
    fi

    weather=$(curl -sf "$API/weather?appid=$KEY&$CITY_PARAM&units=$UNITS")
else
    location=$(curl -sf https://location.services.mozilla.com/v1/geolocate?key=geoclue)

    if [ -n "$location" ]; then
        location_lat="$(echo "$location" | jq '.location.lat')"
        location_lon="$(echo "$location" | jq '.location.lng')"

        weather=$(curl -sf "$API/weather?appid=$KEY&lat=$location_lat&lon=$location_lon&units=$UNITS")
    fi
fi

if [ -n "$weather" ]; then
    weather_icon=$(echo "$weather" | jq -r ".weather[0].icon")
    echo " $(get_icon "$weather_icon")"
fi
