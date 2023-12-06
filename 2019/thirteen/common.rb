# typed: true

PATH_TO_INPUT = File.dirname(__FILE__) + "/input.txt"
PATH_TO_EXAMPLE_INPUT = File.dirname(__FILE__) + "/example-input.txt"

def read_input(path_to_input = PATH_TO_INPUT)
  content = File.read(path_to_input)
  chunks = content.split("\n\n")
  chunks.map do |chunk|
    chunk.strip.split("\n").map do |array_str|
      eval array_str
    end
  end
end
