#!/usr/bin/env bash
# Compare the output of brainfuck after two consecutive runs of this script
# using vimdiff.

bf="dist/build/brainfuck/brainfuck"
dir="/tmp/brainfuck"

infile="$1"

if [[ ! -x "$bf" ]]; then
  echo "brainfuck compiler not found"
elif [[ ! -f "$infile" ]]; then
  echo "usage: compare FILE"
else
  name="$(basename "$infile")"

  old="$dir/$name.c.old"
  new="$dir/$name.c"

  if [[ -f "$new" ]]; then
    mv "$new" "$old"
    $bf -c "$infile" > "$new"
    vimdiff "$new" "$old"
  else
    exec $bf -c "$infile" > "$new"
    echo "Nothing to compare with. Maybe next time..."
  fi
fi
