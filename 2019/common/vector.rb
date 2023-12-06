class Vector
  attr_accessor :row, :col

  def initialize(row, col)
    @row = row
    @col = col
  end

  def x = col
  def y = row

  def +(that) = Vector[@row + that.row, @col + that.col]
  def -(that) = Vector[@row - that.row, @col - that.col]
  def signum = Vector[@row <=> 0, @col <=> 0]
  def zero? = @row == 0 && @col == 0

  def in(min, max)
    @row >= min.row && @row <= max.row && @col >= min.col && @col <= max.col
  end

  def fill_to(that)
    min_row = [@row, that.row].min
    max_row = [@row, that.row].max
    min_col = [@col, that.col].min
    max_col = [@col, that.col].max

    result = []

    for row in min_row..max_row do
      for col in min_col..max_col do
        result << Vector[row, col]
      end
    end

    result
  end

  def self.zero = Vector[0, 0]
  def self.[](row, col) = Vector.new(row, col)

  def self.bounds(vs)
    rows = vs.map(&:row)
    cols = vs.map(&:col)

    [Vector[rows.min, cols.min], Vector[rows.max, cols.max]]
  end

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

