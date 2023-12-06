# typed: true

PATH_TO_INPUT = File.dirname(__FILE__) + "/input.txt"

DIRECTIONS = [
  [0, -1],
  [0, 1],
  [-1, 0],
  [1, 0],
]

def parse_input
  lines = File.readlines(PATH_TO_INPUT)
  lines.map do |line|
    line.strip.chars.map(&:to_i)
  end
end
