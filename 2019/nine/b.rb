# typed: true

instructions = parse_input(PATH_TO_INPUT)

ropes = Array.new(10) do |idx|
  Vector.zero
end

# Populate set with the initial position
visited = Set.new([Vector.zero])

t = 0

for instruction in instructions do
  direction_vector, distance = instruction

  distance.times do |i|
    t += 1

    # Move H
    ropes[0] += direction_vector

    # Move T's
    for upstream_idx in 0...(ropes.size - 1)
      downstream_idx = upstream_idx + 1

      downstream = ropes[downstream_idx]
      upstream = ropes[upstream_idx]

      d = upstream - downstream
      d_unit = d.signum

      # Skip the move of T if we're close enough
      next if d == d_unit

      # Adjust T as needed
      # NB: T moves signum in both directions because
      # we only move if we're outside of the first ring
      # of coordinates. This means both axes can affect
      # the new position of T.
      ropes[downstream_idx] = downstream + d_unit
    end

    # Record T's visited
    added = visited.add?(ropes.last)

    # show(ropes, t, i, distance, direction_vector, visited)
    # sleep(0.01)
  end
end

# show([], nil, nil, nil, nil, visited)

puts 'b: ' + visited.size.to_s
