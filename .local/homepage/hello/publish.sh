#!/bin/bash

key=$(cat key)
city=$(cat city)

dir="/var/www/html/startpage"
sudo cp -Tr . $dir && cd $dir || exit 1

script="script.js"
sudo sed -i "s/_key_/$key/" $script
sudo sed -i "s/_city_/$city/" $script
