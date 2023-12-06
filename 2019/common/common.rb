class Array
  def sliding_window(size)
    this = self
    Array.new(this.size - size + 1) do |i|
      this[i, size]
    end
  end
end
