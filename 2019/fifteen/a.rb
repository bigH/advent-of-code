# typed: true

def manhattan_distance(a, b)
  d = a - b
  d.row.abs + d.col.abs
end

test_row, data, _ = read_input

count = 0

visited_beacons = Set[]
ranges = []

for s in data do
  beacon = s[:beacon]
  sensor = s[:sensor]

  range = manhattan_distance(beacon, sensor)
  distance_to_test_row = (sensor.row - test_row).abs
  range_at_test_row = range - distance_to_test_row

  next if range_at_test_row < 0

  if !visited_beacons.include?(beacon) && beacon.row == test_row
    visited_beacons << beacon
    count -= 1
  end

  # puts "#{beacon}, #{sensor} >> #{range} -> #{range_at_test_row}"
  ranges << [sensor.col - range_at_test_row, sensor.col + range_at_test_row]
end

ranges.sort_by! { |r| r[0] }

min_col = nil
for range in ranges do
  if min_col.nil? || min_col < range[0]
    min_col = range[1]
    count += (range[1] - range[0]) + 1
  elsif min_col >= range[1]
    next
  else # if min_col >= range[0]
    count += (range[1] - min_col)
    min_col = range[1]
  end
end

puts "a: #{count}"
