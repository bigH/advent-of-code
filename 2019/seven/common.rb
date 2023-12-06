# typed: true

PATH_TO_INPUT = File.dirname(__FILE__) + "/input.txt"

def parse_input
  contents = File.read(PATH_TO_INPUT)
  commands = contents.split("$").map(&:strip).reject(&:empty?)
  commands.map do |command_and_output|
    command, *output = command_and_output.split("\n")
    { command: command, output: output } # .tap { |h| p h }
  end
end


