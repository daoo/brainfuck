#!/usr/bin/ruby

def interpret( str, mem = SafeArray.new, pt = 0, doloop = false )
  begin
    i = 0
    while i < str.length do
      case str[i]
        when "+"
          mem.inc( pt )
        when "-"
          mem.dec( pt )
        when ">"
          pt += 1
        when "<"
          pt -= 1
        when ","
          mem[pt] = STDIN.gets[0].ord
        when "."
          print mem.get( pt ).chr
        when "["
          j = i + 1
          while str[j] != "]" do
            j += 1
          end

          mem, pt = interpret( str[i + 1, j - i - 1], mem, pt, true )

          i = j
      end

      i += 1
    end
  end while ( doloop and mem[pt] != 0 )

  return mem, pt
end

class SafeArray < Array
  def inc( i )
    if not self[i]
      self[i] = 0
    end

    self[i] += 1
  end

  def dec( i )
    if not self[i]
      self[i] = 0
    end

    self[i] -= 1
  end

  def get( i )
    if not self[i]
      self[i] = 0
    end

    return self[i]
  end
end

if ARGV[0]
  if File.exist?( ARGV[0] )
    str = ""
    File.open( ARGV[0], "r" ) do |f|
      while line = f.gets
        str << line
      end
    end

    interpret( str )
  else
    interpret( ARGV[0] )
  end
else
  str = STDIN.gets.chop
  interpret( str )
end

puts
