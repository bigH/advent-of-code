# typed: true

grid = parse_input

visibility = Array.new(grid.size) do |index|
  Array.new(grid[index].size, 0x0)
end

for i in 0...grid.size
  j_limit = (grid[i].size - 1)

  tallest_so_far = -1
  for j in (0..j_limit)
    if grid[i][j] <= tallest_so_far
      visibility[i][j] |= 0x1
    else
      tallest_so_far = grid[i][j]
    end
  end

  tallest_so_far = -1
  for j in j_limit.downto(0)
    if grid[i][j] <= tallest_so_far
      visibility[i][j] |= 0x2
    else
      tallest_so_far = grid[i][j]
    end
  end
end

i_limit = (grid.size - 1)

for j in 0...grid[0].size
  tallest_so_far = -1
  for i in (0..i_limit)
    if grid[i][j] <= tallest_so_far
      visibility[i][j] |= 0x4
    else
      tallest_so_far = grid[i][j]
    end
  end

  tallest_so_far = -1
  for i in i_limit.downto(0)
    if grid[i][j] <= tallest_so_far
      visibility[i][j] |= 0x8
    else
      tallest_so_far = grid[i][j]
    end
  end
end

count = 0
for i in 0...visibility.size
  for j in 0...visibility[i].size
    if visibility[i][j] != 0xf
      count += 1
    end
  end
end

puts 'a: ' + count.to_s
