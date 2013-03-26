#!/usr/bin/env bash
# Compile brainfuck to C then compile the C with $CXX or gcc and run it

cc=${CXX:-gcc}
bfc="dist/build/bfc/bfc"
dir="/tmp/brainfuck"

infile="$1"

if [[ ! -x "$bfc" ]]; then
  echo "brainfuck compiler not found"
elif [[ ! -f "$infile" ]]; then
  echo "usage: compile-and-run FILE"
else
  mkdir -p "$dir"

  name="$(basename "$infile" .bf)"

  cfile="$dir/$name.c"
  ofile="$dir/$name"

  $bfc -O2 -t C99 "$infile" > "$cfile"
  $cc -O3 -o "$ofile" "$cfile"
  time exec "$ofile"
fi
