# typed: true

PATH_TO_INPUT = File.dirname(__FILE__) + "/input.txt"
PATH_TO_B_EXAMPLE_INPUT = File.dirname(__FILE__) + "/example-input.txt"

class Vector
  attr_accessor :row, :col

  def initialize(row, col)
    @row = row
    @col = col
  end

  def +(that) = Vector.new(@row + that.row, @col + that.col)
  def -(that) = Vector.new(@row - that.row, @col - that.col)
  def signum = Vector.new(@row <=> 0, @col <=> 0)
  def zero? = @row == 0 && @col == 0

  def self.zero = Vector.new(0, 0)

  def eql?(that)
    self.class == that.class &&
      @row == that.row &&
      @col == that.col
  end
  def ==(that) = eql?(that)

  def inspect = "<#{@row}, #{@col}>"
  def to_s = "<#{@row}, #{@col}>"

  def hash = [@row, @col].hash
end

def direction_as_vector(direction)
  case direction
  when 'D'
    Vector.new(1, 0)
  when 'U'
    Vector.new(-1, 0)
  when 'L'
    Vector.new(0, -1)
  when 'R'
    Vector.new(0, 1)
  else
    raise ArgumentError.new("unsupported direction: #{direction}")
  end
end

def parse_input(input_file = PATH_TO_INPUT)
  lines = File.readlines(input_file)
  lines = lines.map do |line|
    direction, distance = line.strip.split(/\s+/)
    [direction_as_vector(direction), distance.to_i]
  end
end

def show(ropes, t, i, distance, direction_vector, visited)
  rows = [0, *(ropes.map(&:row)), *(visited.map(&:row))]
  min_row = rows.min
  max_row = rows.max

  cols = [0, *(ropes.map(&:col)), *(visited.map(&:col))]
  min_col = cols.min
  max_col = cols.max

  overlappings = {}

  system('clear')

  if [t, i, distance, direction_vector].none?(&:nil?)
    puts "t = #{t} (#{i + 1} of #{distance} -> #{direction_vector})"
    puts ""
  end

  puts "rows = (#{min_row - 1}, #{max_row + 1})"
  puts rows.inspect
  puts "cols = (#{min_col - 1}, #{max_col + 1})"
  puts cols.inspect
  puts ""

  unless ropes.empty?
    puts(ropes.join(' '))
    puts ""
  end

  for row in (min_row - 1)..(max_row + 1) do
    for col in (min_col - 1)..(max_col + 1) do
      v = Vector.new(row, col)

      things_here = []

      ropes.each_index do |index|
        if v == ropes[index]
          things_here << (index == 0 ? 'H' : index.to_s)
        end
      end

      if visited === v
        things_here << '#'
      end

      if v.zero?
        things_here << 's'
      end

      if things_here.empty?
        print '.'
      else
        display_thing, *rest = things_here
        print display_thing
        if ! rest.empty?
          overlappings[display_thing] = rest
        end
      end
    end
    puts ""
  end

  puts ""
  for display, rest in overlappings do
    puts "#{display} overlaps #{rest.join(",")}"
  end

  puts "#{visited.size} visited"
end
