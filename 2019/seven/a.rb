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

def visit_directories(current, sizes)
  total_size = 0
  for key, value in current
    if value.is_a?(Hash)
      total_size += visit_directories(value, sizes)
    else
      total_size += value
    end
  end
  if total_size <= 100_000
    sizes << total_size
  end
  return total_size
end

sizes = []
visit_directories(structure, sizes)
puts 'a: ' + sizes.sum.to_s
