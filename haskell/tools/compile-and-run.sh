#!/usr/bin/env bash

name=$(basename $1)

cfile="/tmp/$name.c"
ofile="/tmp/$name"

$HOME/code/brainfuck/haskell/build/brainfuck -c "$1" > $cfile

gcc -O3 -o "$ofile" "$cfile"

exec "$ofile"

