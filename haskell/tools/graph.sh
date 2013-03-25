#!/usr/bin/env bash

bf="dist/build/brainfuck/brainfuck"
dir="/tmp/brainfuck"

infile="$1"

if [[ ! -x "$bf" ]]; then
  echo "brainfuck compiler not found"
elif [[ ! -f "$infile" ]]; then
  echo "usage: compile-and-run FILE"
else
  mkdir -p "$dir"

  name="$(basename "$infile" .bf)"

  dotfile="$dir/$name.dot"
  svgfile="$dir/$name.svg"

  $bf -O2 -c -t Dot "$infile" > "$dotfile"
  dot "$dotfile" -Tsvg -o"$svgfile"
fi
