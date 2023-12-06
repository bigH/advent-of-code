# typed: true

grid, player, destination = get_input

min_bounds = Vector.zero
max_bounds = Vector[grid.size, grid[0].size]

directions = [
  Vector[0, 1],
  Vector[0, -1],
  Vector[1, 0],
  Vector[-1, 0],
]

queue = [destination]

best_path = {
  destination => 0
}

b_answer = nil

while !queue.empty? && best_path[player].nil? do
  loc = queue.shift
  loc_height = height(grid, loc)

  for direction in directions do
    new_loc = loc + direction

    # out of bounds, invalid path
    next if !new_loc.in(min_bounds, max_bounds)

    # better path already found, prune
    next if best_path.has_key?(new_loc)

    # too high to get to from here, invalid path
    next if loc_height > 1 + height(grid, new_loc)

    # shortest path here is this way
    best_path[new_loc] = best_path[loc] + 1

    if b_answer.nil? && at(grid, new_loc) == 'a'
      b_answer = best_path[new_loc].inspect
    end

    queue.push(new_loc)
  end
end

puts 'a: ' + best_path[player].inspect
puts 'b: ' + b_answer
