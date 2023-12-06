# typed: true

def do_round(monkeys, archetypes, activity_levels, global_mod)
  # puts "-----------"
  monkeys = monkeys.map(&:clone)
  # puts "BEFORE:"
  monkeys.each_index do |monkey_id|
    # puts "#{monkey_id}: #{monkeys[monkey_id].inspect}"
  end
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
      calmed = worried % global_mod
      outcome = test.call(calmed)

      target_monkey = outcomes[outcome ? :true : :false]

      # puts "monkey #{monkey_id}: #{item} -> #{worried} / 3 -> #{calmed} -> #{outcome} -> #{target_monkey}"
      monkeys[target_monkey].append(calmed)
    end
  end
  # monkeys = monkeys.map(&:sort)
  # puts ""
  # puts "AFTER:"
  monkeys.each_index do |monkey_id|
    # puts "#{monkey_id}: #{monkeys[monkey_id].inspect} (#{activity_levels[monkey_id]})"
  end
  return monkeys
end

archetypes = Monkeys
all_mods = archetypes.map { |m| m[:test_number] }
global_mod = all_mods.inject(1, &:*)

monkeys = Monkeys.map { |m| m[:items] }

activity_levels = Array.new(monkeys.size, 0)

round = 1
10000.times do |round|
  # puts "ROUND #{round + 1}" if round % 100 == 99
  monkeys = do_round(monkeys, archetypes, activity_levels, global_mod)
  # sleep(0.1)
end

# puts ""

top_monkeys = activity_levels.sort.reverse[0,2]
# puts "top monkeys #{top_monkeys}"
monkey_business = top_monkeys.inject(1, &:*)

puts 'b: ' + monkey_business.to_s

