#!/usr/bin/env bash
# Compile brainfuck to C then compile the C with gcc and run it

cc=${CXX:-gcc}
bf="build/brainfuck"
dir="/tmp/brainfuck"

infile="$1"

if [[ ! -x "$bf" ]]; then
  echo "brainfuck compiler not found"
elif [[ ! -f "$infile" ]]; then
  echo "usage: compile-and-run FILE"
else
  mkdir -p "$dir"

  name="$(basename "$infile" .bf)"

  cfile="$dir/$name.c"
  ofile="$dir/$name"

  $bf -c "$infile" > "$cfile"
  $cc -O3 -o "$ofile" "$cfile"
  time exec "$ofile"
fi
