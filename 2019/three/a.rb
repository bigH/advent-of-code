# typed: true

File.open(PATH_TO_INPUT, 'r') do |file|
  total = 0
  file.each_line do |line|
    ruck_contents = line.strip
    half_ruck_length = ruck_contents.size / 2
    assert half_ruck_length * 2 == ruck_contents.size
    first, second = ruck_contents[0...half_ruck_length], ruck_contents[half_ruck_length..-1]
    # puts "#{ruck_contents} / #{first} / #{second}"
    assert first.size == half_ruck_length
    assert first.size == second.size

    items_in_first = first.chars.uniq
    items_in_second = second.chars.uniq

    intersection = items_in_first & items_in_second

    total += calculate_priority(intersection[0])
  end
  puts 'a: ' + total.to_s
end
