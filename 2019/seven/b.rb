# typed: true

commands = parse_input

structure = {}
current_stack = []

for execution in commands
  command = execution[:command]

  command_parts = command.split(" ")

  if command_parts[0] == 'cd'
    if command_parts[1] == '..'
      current_stack.pop
    elsif command_parts[1] == '/'
      current_stack = []
    else
      current_stack << command_parts[1]
    end
  end

  if command_parts[0] == 'ls'
    output = execution[:output]

    directory_hash = structure

    for directory in current_stack
      directory_hash = directory_hash[directory] ||= {}
    end

    for line in output
      parts = line.split(" ")
      if parts[0] == 'dir'
        directory_hash[parts[1]] ||= {}
      else
        directory_hash[parts[1]] = parts[0].to_i
      end
    end
  end
end

def print_structure(current, indent = 0)
  for key, value in current
    if value.is_a?(Hash)
      puts '  ' * indent + key + '/'
      print_structure(value, indent + 1)
    else
      puts '  ' * indent + key + ' ' + value.to_s
    end
  end
end

def visit_directories(current)
  sizings = {}
  total_size = 0
  for node_name, value in current
    if value.is_a?(Hash)
      total_dir_size, sub_sizings = visit_directories(value)
      sub_sizings.each do |sub_path, sub_size|
        sizings[node_name + '/' + sub_path] = sub_size
      end
      sizings[node_name + '/'] = total_dir_size
      total_size += total_dir_size
    else
      sizings[node_name] = value
      total_size += value
    end
  end
  return total_size, sizings
end

# print_structure(structure)

total_size, sizings = visit_directories(structure)
sizings['/'] = total_size

desired_space = 30000000
available_space = 70000000 - total_size

space_to_free = desired_space - available_space

best_dir_size = 70000000
for path, path_size in sizings
  # puts path + ': ' + path_size.inspect
  if path_size >= space_to_free && path_size < best_dir_size
    best_dir_size = path_size
  end
end

puts 'b: ' + best_dir_size.to_s
