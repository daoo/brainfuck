#!/usr/bin/env bash

bf="build/brainfuck"
dir="/tmp/brainfuck"

if [[ ! -x "$bf" ]]; then
  echo "brainfuck compiler not found"
elif [[ ! -f "$1" ]]; then
  echo "usage: compile-and-run brainfuck.bf"
else
  mkdir -p "$dir"

  name="$(basename "$1" .bf)"

  cfile="$dir/$name.c"
  ofile="$dir/$name"

  $bf -c "$1" > "$cfile"

  gcc -O3 -o "$ofile" "$cfile"

  exec "$ofile"
fi
