# typed: true

PATH_TO_INPUT = File.dirname(__FILE__) + "/input.txt"
PATH_TO_EXAMPLE_INPUT = File.dirname(__FILE__) + "/example-input.txt"

def read_input(path_to_input = PATH_TO_INPUT)
  content = File.read(path_to_input)
  lines = content.split("\n").map(&:strip).reject(&:empty?)
  lines.map do |line|
    line.split(" -> ").map do |pair|
      col, row = pair.split(",")
      Vector[row.to_i, col.to_i]
    end
  end
end

def bounds(trajectories)
  points = trajectories.flat_map(&:itself)
  Vector.bounds([Vector[0, 500], *points])
end

def build_grid_with_only_trajectories(trajectories)
  min, max = bounds(trajectories)
  grid = Grid.new(min, max)
  trajectories.each do |trajectory|
    pairs = trajectory.sliding_window(2)
    # puts pairs.inspect
    pairs.each do |a, b|
      # puts "drawing #{a} #{b}"
      grid.add_barrier(a, b)
    end
  end
  grid
end
