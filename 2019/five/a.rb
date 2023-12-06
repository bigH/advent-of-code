# typed: true

stacks, instructions = parse_input

instructions.each do |instruction|
  from = instruction[:from].to_i
  to = instruction[:to].to_i
  count = instruction[:count].to_i

  count.times do |i|
    stacks[to] << stacks[from].pop
  end
end

print 'a: '
stacks.each do |id, stack|
  print stack.last
end
puts
