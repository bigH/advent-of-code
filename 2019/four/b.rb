# typed: true

File.open(PATH_TO_INPUT, 'r') do |file|
  count = 0
  file.each_line do |line|
    abs, aes, bbs, bes = line.strip.split(/[-,]/)
    ab, ae, bb, be = abs.to_i, aes.to_i, bbs.to_i, bes.to_i
    if ((ab <= be) && (ae >= bb)) ||
      ((bb <= ae) && (be >= ab)) ||
      ((ab <= bb) && (ae >= be)) ||
      ((bb <= ab) && (be >= ae))
      count += 1
    end
  end
  puts 'b: ' + count.to_s
end
