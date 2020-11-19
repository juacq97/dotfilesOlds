#!/usr/bin/env bash

shopt -s nullglob globstar

typeit=0
if [[ $1 == "--type" ]]; then
	typeit=1
	shift
fi


STARTDIR=${PASSWORD_STORE_DIR-~/.password-store}
BASEDIR=$STARTDIR
DONE=0
LEVEL=0
PREVSELECTION=""
SELECTION=""

while [ "$DONE" -eq 0 ] ; do 
  password_files=( "$STARTDIR"/* )
  password_files=( "${password_files[@]#"$STARTDIR"/}" )
  password_files=( "${password_files[@]%.gpg}" )
  
  if [ "$LEVEL" -ne 0 ] ; then
    password_files=(".." "${password_files[@]}") 
  fi
  entry=$(printf '%s\n' "${password_files[@]}" | dmenu "$@" -l 15)
  
  echo "entry: $entry"
  if [ -z "$entry" ] ; then
    DONE=1
    exit
  fi
  
  if [ "$entry" != ".." ] ; then
    PREVSELECTION=$SELECTION
    SELECTION="$SELECTION/$entry"
  
    # check if another dir
    if [ -d "$STARTDIR/$entry" ] ; then
      STARTDIR="$STARTDIR/$entry"
      LEVEL=$((LEVEL+1))
    else
      # not a directory so it must be a real password entry
  
      if [[ $typeit -eq 0 ]]; then
        pass show -c "$SELECTION" 2>/dev/null
      else
        xdotool - <<<"type --clearmodifiers -- $(pass show "$SELECTION" | head -n 1)"
      fi
      DONE=1
    fi
  
  else
    LEVEL=$((LEVEL-1))
    SELECTION=$PREVSELECTION
    STARTDIR="$BASEDIR/$SELECTION"
  fi
done

