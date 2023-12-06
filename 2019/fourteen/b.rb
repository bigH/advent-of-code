# typed: true

def step(grid, sand_position)
  preferred_delta = TRIAL_ORDER.find do |delta|
    grid[sand_position + delta] == '.'
  end

  preferred_delta ||= Vector.zero

  sand_position + preferred_delta
end

trajectories = read_input(PATH_TO_EXAMPLE_INPUT)
min, max = bounds(trajectories)
floor_row = max.row + 2
floor = [Vector[floor_row, 500 - floor_row], Vector[floor_row, 500 + floor_row]]
# puts floor.inspect
trajectories << floor
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
  sleep (0.02)
  system('clear')
  puts "t = #{time}" # if time % 1000 == 0
  puts "s = #{grid.count('o')}" # if time % 1000 == 0
  puts ""
  grid.render
  if new_sand_position == sand_spawn_point
    break
  elsif new_sand_position == sand_position
    grid.clear('~')
    sand_count += 1
    sand_position = sand_spawn_point
    grid.set(sand_position, 'o')
  elsif ! new_sand_position.in(min_bound, max_bound)
    raise Exception.new("sand fell off the screen")
    # sand fell off the screen, meaning we're done
    # we still need to make sure `clear` will work properly
    break
  else
    grid.set(sand_position, '~')
    grid.set(new_sand_position, 'o')
    sand_position = new_sand_position
  end
  time += 1
end

puts "b: #{grid.count('o')}"
