# typed: true

path_to_input = File.dirname(__FILE__) + "/input.txt"

File.open(path_to_input, 'r') do |file|
  score = 0
  file.each_line do |line|
    theirs, desire = line.strip.split(/\s+/)
    their_pick = THEIRS[theirs]
    desired_outcome = DESIRE[desire]

    my_pick = calculate_move_for(their_pick, desired_outcome)

    pick_score = SCORE[my_pick]

    win_score = 0
    if my_pick == their_pick
      win_score = 3
    elsif did_i_win(my_pick, their_pick)
      win_score = 6
    end

    score += pick_score + win_score
  end
  puts 'b: ' + score.to_s
end
