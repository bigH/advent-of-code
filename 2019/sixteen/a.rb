# typed: true

valves = read_input(PATH_TO_EXAMPLE_INPUT)

simplified = simplify_graph(valves)

simplified.each do |k, v|
  puts "#{k} -> #{v.inspect}"
end
