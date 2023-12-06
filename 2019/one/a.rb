# typed: true

path_to_input = File.dirname(__FILE__) + "/input.txt"

File.open(path_to_input, 'r') do |file|
  elves = []
  total_so_far = 0
  file.each_line do |line|
    if line.strip == ''
      elves << total_so_far
      total_so_far = 0
    else
      total_so_far += line.strip.to_i
    end
  end
  puts 'a: ' + elves.max.to_s
end
