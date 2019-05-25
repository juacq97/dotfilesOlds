#! /bin/sh

if [ "$(pidof redshift)" ]
then
    killall redshift
    echo "       "
else
    redshift &
    echo "       "
fi
