# typed: true

def should_check_cycle(time) = time % 40 == 20

def check(signal_strengths, registers, time)
  # puts "Checking at time #{time}"
  if should_check_cycle(time)
    # puts "Calculating #{time}"
    signal_strengths << (registers[:X] * time)
  end
end

def main()
  registers = { X: 1 }
  time = 0
  program_counter = 0

  signal_strengths = []

  instructions = parse_input
  # instructions = parse_input(PATH_TO_EXAMPLE_INPUT)

  for instruction in instructions do
    if instruction == :noop
      time += 1
      check(signal_strengths, registers, time)
    else
      time += 1
      check(signal_strengths, registers, time)
      time += 1
      check(signal_strengths, registers, time)
      registers[:X] += instruction
    end
  end

  # puts signal_strengths.inspect
  puts 'a: ' + signal_strengths.sum.to_s
end

main()
