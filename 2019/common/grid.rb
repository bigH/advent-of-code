class Grid
  attr_reader :min_bound
  attr_reader :max_bound
  attr_reader :infinite

  def initialize(min, max, infinite = true)
    @infinite = infinite
    @min_bound = min
    @max_bound = max

    @grid = Array.new((max.row - min.row) + 1) do |row|
      Array.new((max.col - min.col) + 1) do |col|
        '.'
      end
    end
  end

  def add_barrier(a, b)
    for v in a.fill_to(b)
      set(v, '#')
    end
  end

  def [](v)
    at(v)
  end
  def []=(v, setting)
    set(v, setting)
  end

  def at(v)
    if v.in(min_bound, max_bound)
      shifted = v - min_bound
      @grid[shifted.row][shifted.col]
    elsif @infinite
      return '.'
    else
      raise Exception.new("Out of bounds: #{v} (#{min_bound} - #{max_bound})")
    end
  end

  def set(v, setting)
    shifted = v - min_bound
    @grid[shifted.row][shifted.col] = setting
  end

  def clear(setting, to = '.')
    @grid.each_index do |row|
      @grid[row].each_index do |col|
        if @grid[row][col] == setting
          @grid[row][col] = to
        end
      end
    end
  end

  def count(setting)
    @grid.inject(0) do |acc, row|
      acc + row.count do |el|
        el == setting
      end
    end
  end

  def render
    puts "min_bound = #{min_bound}"
    puts "max_bound = #{max_bound}"
    puts ""
    @grid.each_index do |row|
      @grid[row].each_index do |col|
        thing = @grid[row][col]
        if thing == '#'
          print green(thing)
        elsif thing == 'o'
          print red(thing)
        elsif thing == '~'
          print yellow(thing)
        else
          print gray(thing)
        end
      end
      puts ""
    end
  end
end

