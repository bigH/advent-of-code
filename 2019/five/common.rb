# typed: true

PATH_TO_INPUT = File.dirname(__FILE__) + "/input.txt"

def parse_input
  lines = File.readlines(PATH_TO_INPUT)
  lines = lines.map { |line| line.gsub("\n",'') }
  stacks, instructions = split_on_newline(lines)
  stacks = build_stacks_from_input(stacks)
  instructions = parse_instructions(instructions)
  [stacks, instructions]
end

def split_on_newline(lines)
  delimiter_index = lines.find_index { |line| line.strip == '' }
  post_delimiter = (delimiter_index + 1)
  [lines[0, delimiter_index], lines[post_delimiter..-1]]
end

def build_stacks_from_input(stacks)
  stacks = stacks.map(&:chars)
  new_stacks = {}
  stacks.transpose.each do |possible_stack|
    possible_id, *rest = possible_stack.reverse
    if possible_id.to_i != 0
      new_stacks[possible_id.to_i] = rest.take_while { |c| c != ' ' }
    end
  end
  new_stacks
end

def parse_instructions(instructions)
  instructions.map do |instruction|
    instruction[/move (\d+) from (\d+) to (\d+)/]
    { from: $2.to_i, to: $3.to_i, count: $1.to_i }
  end
end

