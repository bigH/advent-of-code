# typed: true

PATH_TO_INPUT = File.dirname(__FILE__) + "/input.rb"
PATH_TO_EXAMPLE_INPUT = File.dirname(__FILE__) + "/example-input.rb"

def read_input(path_to_input = PATH_TO_INPUT)
  sensor_data = File.read(path_to_input)
  eval(sensor_data)
end
