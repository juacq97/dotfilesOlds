#!/bin/sh

dia=$(date +"%d")

cal | sed "s/$dia/\${color E13CCD}$dia\${color}/g"
