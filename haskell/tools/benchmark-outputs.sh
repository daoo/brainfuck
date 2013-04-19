#!/usr/bin/env bash
# Benchmark the generated haskell and c code

maxlines=${MAXLINES:-2000}
cc=${CXX:-gcc}
hc="ghc -outputdir '/tmp' -isrc"
bfc="dist/build/bfc/bfc"
dir="/tmp/brainfuck"

infile="$1"

if [[ ! -x "$bfc" ]]; then
  echo "brainfuck compiler not found"
elif [[ ! -f "$infile" ]]; then
  echo "usage: benchmark-output FILE"
else
  mkdir -p "$dir"

  name="$(basename "$infile" .bf)"

  cfile="$dir/$name.c"
  hsfile="$dir/$name.hs"
  ofile="$dir/$name"

  $bfc -O2 -t C99 "$infile" > "$cfile"
  $bfc -O2 -t Haskell "$infile" > "$hsfile"
  $cc -O3 -o "$ofile-c" "$cfile" > /dev/null
  $hc -O2 -o "$ofile-haskell" "$hsfile" > /dev/null

  echo "exec $ofile-haskell"
  time exec "$ofile-haskell" | head -n $maxlines > /dev/null

  echo
  echo "exec $ofile-c"
  time exec "$ofile-c" | head -n $maxlines > /dev/null
fi
