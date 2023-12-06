# typed: true

PATH_TO_INPUT = File.dirname(__FILE__) + "/input.txt"
PATH_TO_EXAMPLE_INPUT = File.dirname(__FILE__) + "/example-input.txt"

def parse_input(input_file = PATH_TO_INPUT)
  lines = File.readlines(input_file)
  lines.map do |line|
    instruction = line.strip
    if instruction == 'noop'
      :noop
    else
      instruction.split.last.to_i
    end
  end
end
