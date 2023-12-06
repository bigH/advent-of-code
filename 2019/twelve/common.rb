# typed: true

PATH_TO_INPUT = File.dirname(__FILE__) + "/input.txt"
PATH_TO_EXAMPLE_INPUT = File.dirname(__FILE__) + "/example-input.txt"

def height(grid, loc)
  at(grid, loc).ord - 'a'.ord
end

def at(grid, loc)
  grid[loc.row][loc.col]
end

def get_input(path_to_input = PATH_TO_INPUT)
  contents = File.read(path_to_input)
  lines = contents.split("\n")
  stripped = lines.map(&:strip)
  valid = stripped.reject(&:empty?)
  grid = valid.map(&:chars)

  player = nil
  destination = nil

  grid.each_index do |i|
    grid[i].each_index do |j|
      if grid[i][j] == 'S'
        player = Vector[i, j]
        grid[i][j] = 'a'
      end
      if grid[i][j] == 'E'
        destination = Vector[i, j]
        grid[i][j] = 'z'
      end
    end
  end

  [grid, player, destination]
end
