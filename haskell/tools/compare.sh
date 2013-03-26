#!/usr/bin/env bash
# Compare the output of brainfuck after two consecutive runs of this script
# using vimdiff.

bfc="dist/build/bfc/bfc"
dir="/tmp/brainfuck"

infile="$1"

if [[ ! -x "$bfc" ]]; then
  echo "brainfuck compiler not found"
elif [[ ! -f "$infile" ]]; then
  echo "usage: compare FILE"
else
  name="$(basename "$infile")"

  old="$dir/$name.c.old"
  new="$dir/$name.c"

  if [[ -f "$new" ]]; then
    mv "$new" "$old"
    $bfc -O2 -t C99 "$infile" > "$new"
    vimdiff "$new" "$old"
  else
    exec $bfc -O2 -t C99 "$infile" > "$new"
    echo "Nothing to compare with. Maybe next time..."
  fi
fi
