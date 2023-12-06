# typed: true

def check(registers, time, drawing)
  position = (time - 1) % 40
  add_to_drawing = 
    if (position - registers[:X]).abs <= 1
      '#'
    else
      '.'
    end

  if time % 40 == 0
    add_to_drawing += "\n"
  end

  drawing += add_to_drawing

  # puts '--------------------------'
  # puts "t = #{time}"
  # puts "X = #{registers[:X]}"
  # puts "drawing = \n#{drawing}"
  # puts ''

  drawing
end

def main()
  registers = { X: 1 }
  time = 0
  program_counter = 0

  instructions = parse_input
  drawing = ""

  for instruction in instructions do
    if instruction == :noop
      time += 1
      drawing = check(registers, time, drawing)
    else
      time += 1
      drawing = check(registers, time, drawing)
      time += 1
      drawing = check(registers, time, drawing)
      registers[:X] += instruction
      # puts "X = #{registers[:X]}"
      # puts ""
    end
  end

  puts 'b: '
  puts drawing
end

main()
