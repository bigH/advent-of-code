# typed: true

PATH_TO_INPUT = File.dirname(__FILE__) + "/input.rb"
PATH_TO_EXAMPLE_INPUT = File.dirname(__FILE__) + "/example-input.rb"

def read_input(path_to_input = PATH_TO_INPUT)
  valve_map = File.read(path_to_input)
  eval(valve_map)
end

def simplify_graph(valves)
  new_graph = {}

  for key, value in valves do
    if value[:rate] == 0
      # exclude all 0 rate valves, since we're going to connect through them
      next if key != 'AA'
    end

    # get simplified edges
    all_edges = simlify_edges_from(valves, key)

    # make sure to exclude any edges where there's no useful valve
    edges = all_edges.filter do |e, d|
      valves[e][:rate] != 0
    end

    new_graph[key] = value.merge({edges: edges})
  end

  return new_graph
end

def simplify_edges_from(valves, key)
  root = key
  queue = [[key, 0]]

  route_scores = { key => 0 }

  loop do
    break if queue.empty?

    current_key, distance = queue.shift

    # NB: we know that if we've visited it already,
    # there's a faster path there OR a cycle
    break if route_scores.has_key?(current_key)

    current = valves[current_key]
    current_edges = current[:connects_to]

    d = distance + 1
    queue += current_edges.map do |e|
               [e, d]
             end
  end

  return route_scores
end

