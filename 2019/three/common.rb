# typed: true

def assert(*args, &block)
  begin
    if block_given?
      result = yield
    elsif args.size == 1
      result = args[0]
    else
      raise "Invalid arguments"
    end

    unless result
      raise "Assertion failed"
    end
  rescue Exception => e
    puts e
    raise "Threw exception"
  end
end

def calculate_priority(char)
  if char.ord <= 'Z'.ord
    char.ord - 'A'.ord + 27
  else
    char.ord - 'a'.ord + 1
  end
end

PATH_TO_INPUT = File.dirname(__FILE__) + "/input.txt"
