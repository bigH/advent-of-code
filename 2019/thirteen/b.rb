# typed: true

def is_array(el) = el.is_a?(Array)
def coerce_to_array(el) = is_array(el) ? el.clone : [el]

def fill_to(list, level)
  (level - list.length).times do
    list << nil
  end
end

# 1 == ascending, 0 == same, -1 == descending
def compare(l, r, i = 0)
  if l.nil? && r.nil?
    return 0
  elsif l.nil?
    return -1
  elsif r.nil?
    return 1
  else
    if !is_array(l) && !is_array(r)
      return l <=> r
    else
      l_prime = coerce_to_array(l)
      r_prime = coerce_to_array(r)
      fill_level = [l_prime.size, r_prime.size].max
      fill_to(l_prime, fill_level)
      fill_to(r_prime, fill_level)
      for l_el, r_el in l_prime.zip(r_prime) do
        el = compare(l_el, r_el, i + 1)
        if el != 0
          return el
        end
      end
      return 0
    end
  end
end

pairs = read_input

lists = pairs.flat_map(&:itself)
lists << [[2]]
lists << [[6]]

lists.sort! { |a, b|
  compare(a, b)
}

product = 1
lists.each_index do |i|
  if lists[i] == [[2]]
    product *= (i + 1)
  end

  if lists[i] == [[6]]
    product *= (i + 1)
  end
end

puts "b: #{product}"
