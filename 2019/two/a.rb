# typed: true

R = :rock
P = :paper
S = :scissors

SCORE = {
  R => 1,
  P => 2,
  S => 3,
}

DESIRE = {
  'X' => :lose,
  'Y' => :draw,
  'Z' => :win,
}

MINE = {
  'X' => R,
  'Y' => P,
  'Z' => S,
}

THEIRS = {
  'A' => R,
  'B' => P,
  'C' => S,
}

def did_i_win(my_pick, their_pick)
  case my_pick
  when R
    their_pick == S
  when P
    their_pick == R
  when S
    their_pick == P
  end
end

def calculate_move_for(their_pick, desired_outcome)
  case desired_outcome
  when :win
    case their_pick
    when P
      S
    when R
      P
    when S
      R
    end
  when :lose
    case their_pick
    when S
      P
    when P
      R
    when R
      S
    end
  when :draw
    their_pick
  end
end

path_to_input = File.dirname(__FILE__) + "/input.txt"

File.open(path_to_input, 'r') do |file|
  score = 0
  file.each_line do |line|
    theirs, mine = line.strip.split(/\s+/)
    their_pick = THEIRS[theirs]
    my_pick = MINE[mine]

    pick_score = SCORE[my_pick]

    win_score = 0
    if my_pick == their_pick
      win_score = 3
    elsif did_i_win(my_pick, their_pick)
      win_score = 6
    end

    score += pick_score + win_score
  end
  puts 'a: ' + score.to_s
end
