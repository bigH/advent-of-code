# typed: true

File.open(PATH_TO_INPUT, 'r') do |file|
  total = 0
  rucks = file.readlines.map(&:strip)
  groups = []
  rucks.each_with_index do |ruck, index|
    group = groups[index / 3] ||= []
    group << ruck
  end

  groups.each do |group|
    a, b, c = group
    a_chars = a.chars.uniq
    b_chars = b.chars.uniq
    c_chars = c.chars.uniq

    badge = a_chars & b_chars & c_chars

    assert badge.size == 1

    total += calculate_priority(badge[0])
  end

  puts 'b: ' + total.to_s
end
