#!/usr/bin/env bash
# Launch a gvim then compile and run the file when saved

cc=${CXX:-gcc}
dir="/tmp/brainfuck/"
input_file="$dir/input"
bfc="dist/build/bfc/bfc"

bf_file="$1"
bf_input="$2"

if [[ ! -x "$bfc" ]]; then
  echo "brainfuck compiler not found"
elif [[ ! -f "$bf_file" ]]; then
  echo "usage: interactive-testing FILE INPUT"
else
  mkdir -p $dir

  name="$(basename $bf_file .bf)"
  cfile="$dir/$name.c"
  ofile="$dir/$name"

  $bfc -O2 -t C99 "$bf_file" > "$cfile"
  gvim --class brainfuck "$cfile" &

  while inotifywait -qe close_write,moved_to,create "$dir"; do
    $cc -O3 -o "$ofile" "$cfile"
    echo -ne "$bf_input\000" > "$input_file"
    time "$ofile" < "$input_file" | head -n20
    sleep 1
  done
fi
