# typed: true

instructions = parse_input

h = Vector.zero
t = Vector.zero
d = Vector.zero

# Populate set with the initial position
visited = Set.new([Vector.zero])

for instruction in instructions do
  direction_vector, distance = instruction
  distance.times do |i|
    # Move H
    h += direction_vector

    # Calculate (H-T)
    d = h - t

    # Skip the move of T if we're close enough
    next if d == d.signum

    # Adjust T as needed
    # NB: T moves signum in both directions because
    # we only move if we're outside of the first ring
    # of coordinates. This means both axes can affect
    # the new position of T.
    t += d.signum

    # Record T's visited
    added = visited.add?(t)
    # if added.nil?
    #   puts "revisiting #{t.inspect}"
    # else
    #   puts "visiting #{t.inspect}"
    # end
  end
end

puts 'a: ' + visited.size.to_s
