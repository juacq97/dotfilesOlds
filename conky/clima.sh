#! /bin/sh

icon=$(weather-icon.sh)
number=$(openweathermap-detailed.sh)
echo $icon $number
