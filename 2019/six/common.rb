# typed: true

PATH_TO_INPUT = File.dirname(__FILE__) + "/input.txt"

def parse_input
  content = File.read(PATH_TO_INPUT)
  content.chars.to_a
end
