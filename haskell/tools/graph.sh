#!/usr/bin/env bash
# Generate a SVG graph of the Taprit AST

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

  dotfile="$dir/$name.dot"
  svgfile="$dir/$name.svg"

  $bfc -O2 -t Dot "$infile" > "$dotfile"
  dot "$dotfile" -Tsvg -o"$svgfile"
fi
