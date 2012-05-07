#!/usr/bin/env bash

bf="build/brainfuck"
infile="$1"

if [[ ! -x "$bf" ]]; then
  echo "brainfuck compiler not found"
elif [[ ! -f "$infile" ]]; then
  echo "usage: comapre brainfuck.bf"
else
  name="$(basename "$infile")"
  
  old="/tmp/$name.c.old"
  new="/tmp/$name.c"

  if [[ -f "$new" ]]; then
    mv "$new" "$old"
    $bf -c "$1" > "$new"
    vimdiff "$new" "$old"
  else
    $bf -c "$1" > "$new"
    echo "Nothing to compare with. Maybe next time..."
  fi
fi
