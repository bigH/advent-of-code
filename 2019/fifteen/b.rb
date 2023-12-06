# typed: true

def manhattan_distance(a, b)
  d = a - b
  d.row.abs + d.col.abs
end

_, data, max = read_input

min_bound = Vector.zero
max_bound = Vector[max, max]

point = nil

for s in data do
  s[:range] = manhattan_distance(s[:sensor], s[:beacon])
end

for ring_sensor in data do
  break if !point.nil?

  sensor = ring_sensor[:sensor]
  beacon = ring_sensor[:beacon]
  range = ring_sensor[:range]

  other_sensors = data - [ring_sensor]
  other_sensors = other_sensors.filter do |s|
    distance = manhattan_distance(sensor, s[:sensor])
    distance <= (range + s[:range] + 1)
  end

  test_point_distance = range + 1

  for row_delta in 0..test_point_distance do
    break if !point.nil?

    col_delta = test_point_distance - row_delta

    deltas = [
      Vector[-row_delta, -col_delta],
      Vector[-row_delta,  col_delta],
      Vector[ row_delta, -col_delta],
      Vector[ row_delta,  col_delta],
    ]

    for delta in deltas do
      break if !point.nil?

      shifted = sensor + delta

      next if !shifted.in(min_bound, max_bound)

      out_of_range = other_sensors.all? do |test|
        test_sensor = test[:sensor]
        test_range = test[:range]
        manhattan_distance(test_sensor, shifted) > test_range
      end

      if out_of_range
        point = shifted
      end
    end
  end
end

tuning_freq = point.col * 4000000 + point.row
puts "b: #{tuning_freq}"
