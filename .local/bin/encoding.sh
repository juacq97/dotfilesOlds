#! /bin/bash

f="ISO-8859-14"
t="UTF-8"

convert=" iconv -f $f -t $t"

for file in *.fortunes; do
    $convert "$file" -o "${file%.fortunes}.converted"
done
exit 0
