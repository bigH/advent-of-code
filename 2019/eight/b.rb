# typed: true

grid = parse_input

scores = Array.new(grid.size) do |index|
  Array.new(grid[index].size, 0)
end

def calculate_scenic_score(grid, i, j)
  tree_height = grid[i][j]
  product = 1

  DIRECTIONS.each do |direction|
    di, dj = direction
    ci, cj = i, j
    visible_trees = 0

    loop do
      ci = ci + di
      cj = cj + dj

      break unless ci >= 0 && ci < grid.size && cj >= 0 && cj < grid[ci].size

      if grid[ci][cj].nil?
        puts "#{ci} #{cj} #{grid[ci]} #{grid[ci][cj] unless grid[ci].nil?}"
      end

      if grid[ci][cj] <= tree_height
        visible_trees += 1
      end

      if grid[ci][cj] >= tree_height
        break
      end
    end

    product *= visible_trees
  end
  return product
end

for i in 0...grid.size
  for j in 0...(grid[i].size)
    score = calculate_scenic_score(grid, i, j)
    scores[i][j] = score
  end
end

puts 'b: ' + scores.map(&:max).max.to_s

