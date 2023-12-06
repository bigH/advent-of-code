# typed: true

signal_bytes = parse_input

for i in 13...signal_bytes.length
  sub_array = signal_bytes[i-13..i]
  if sub_array.uniq.length == sub_array.length
    puts "a: msg packet ending at #{i + 1}"
    break
  end
end
