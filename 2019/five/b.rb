# typed: true

stacks, instructions = parse_input

instructions.each do |instruction|
  from = instruction[:from].to_i
  to = instruction[:to].to_i
  count = instruction[:count].to_i

  temp = []
  count.times do |i|
    temp << stacks[from].pop
  end
  count.times do |i|
    stacks[to] << temp.pop
  end
end

print 'b: '
stacks.each do |id, stack|
  print stack.last
end
puts
