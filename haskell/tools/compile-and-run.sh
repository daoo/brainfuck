#!/usr/bin/env bash

bf="build/brainfuck"

if [[ ! -x "$bf" ]]; then
  echo "brainfuck compiler not found"
elif [[ ! -f "$1" ]]; then
  echo "usage: compile-and-run brainfuck.bf"
else
  name="$(basename "$1" .bf)"

  cfile="/tmp/$name.c"
  ofile="/tmp/$name"

  $bf -c "$1" > "$cfile"

  gcc -O3 -o "$ofile" "$cfile"

  exec "$ofile"
fi
