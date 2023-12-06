# typed: true

def do_round(monkeys, archetypes, activity_levels)
  # puts "-----------"
  monkeys = monkeys.map(&:clone)
  # puts "BEFORE:"
  # monkeys.each_index do |monkey_id|
  #   puts "#{monkey_id}: #{monkeys[monkey_id].inspect}"
  # end
  # puts ""
  monkeys.each_index do |monkey_id|
    items = monkeys[monkey_id]
    archetype = archetypes[monkey_id]

    operation = archetype[:operation]
    test = archetype[:test]
    outcomes = archetype[:outcomes]

    while ! items.empty? do
      item = items.shift

      activity_levels[monkey_id] += 1

      worried = operation.call(item)
      calmed = worried / 3
      outcome = test.call(calmed)

      target_monkey = outcomes[outcome ? :true : :false]

      # puts "monkey #{monkey_id}: #{item} -> #{worried} / 3 -> #{calmed} -> #{outcome} -> #{target_monkey}"
      monkeys[target_monkey].append(calmed)
    end
  end
  # puts ""
  # puts "AFTER:"
  # monkeys.each_index do |monkey_id|
  #   puts "#{monkey_id}: #{monkeys[monkey_id].inspect} (#{activity_levels[monkey_id]})"
  # end
  return monkeys
end

archetypes = Monkeys

monkeys = Monkeys.map { |m| m[:items] }

activity_levels = Array.new(monkeys.size, 0)

round = 1
20.times do |round|
  # system('clear')
  # puts "ROUND #{round + 1}"
  monkeys = do_round(monkeys, archetypes, activity_levels)
  # sleep(0.1)
  # gets
end

# puts ""

top_monkeys = activity_levels.sort.reverse[0,2]
# puts "top monkeys #{top_monkeys}"
monkey_business = top_monkeys.inject(1, &:*)

puts 'a: ' + monkey_business.to_s
