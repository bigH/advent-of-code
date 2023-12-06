# typed: true

def step(grid, sand_position)
  preferred_delta = TRIAL_ORDER.find do |delta|
    grid[sand_position + delta] == '.'
  end

  preferred_delta ||= Vector.zero

  sand_position + preferred_delta
end

trajectories = read_input
grid = build_grid_with_only_trajectories(trajectories)

min_bound = grid.min_bound
max_bound = grid.max_bound

time = 0

sand_spawn_point = Vector[0, 500]

sand_count = 0
sand_position = sand_spawn_point
grid.set(sand_position, 'o')

loop do
  new_sand_position = step(grid, sand_position)
  # sleep (0.05)
  # system('clear')
  # puts "t = #{time}"
  # puts "s = #{grid.count('o')}"
  # puts ""
  # grid.render
  # puts ""
  # puts "sand #{sand_position} -> #{new_sand_position}"
  if new_sand_position == sand_position
    grid.clear('~')
    sand_count += 1
    sand_position = sand_spawn_point
    grid.set(sand_position, 'o')
  elsif ! new_sand_position.in(min_bound, max_bound)
    # sand fell off the screen, meaning we're done
    # we still need to make sure `clear` will work properly
    grid.set(sand_position, '~')
    break
  else
    grid.set(sand_position, '~')
    grid.set(new_sand_position, 'o')
    sand_position = new_sand_position
  end
  time += 1
end

puts "a: #{grid.count('o')}"
