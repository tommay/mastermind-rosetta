#!/usr/bin/env ruby

at_exit{main}

#CHOICES = [1, 2, 3, 4, 5, 6]
#CODE_SIZE = 4
CHOICES = [1, 2, 3, 4, 5]
CODE_SIZE = 4

make_codes = lambda do |choices, size|
  if size == 0
    [[]]
  else
    codes = make_codes.call(choices, size - 1)
    choices.flat_map do |choice|
      codes.map {|code| [choice] + code}
    end
  end
end

ALL_CODES = make_codes.call(CHOICES, CODE_SIZE)

def main
  starting_guesses = ALL_CODES.uniq {|c| categorize(c)}
  starting_guesses.each do |guess|
    category = categorize(guess)
    stats = compute_stats(guess)
    puts "#{category} #{stats.to_s}"
  end
end

# => Stats
#
def compute_stats(starting_guess)
  ALL_CODES.reduce(Stats.new) do |stats, code|
    fold_guess(code, 1, ALL_CODES, stats, starting_guess)
  end
end

# => Stats
#
def fold_guess(code, depth, possibilities, stats, guess)
  score = compute_score(guess, code)
  if score.first == CODE_SIZE
    stats.solved(depth)
  else
    remaining_possibilities = possibilities.select do |possible_code|
      compute_score(guess, possible_code) == score
    end
    remaining_possibilities.reduce(stats) do |stats, guess|
      fold_guess(code, depth + 1, remaining_possibilities, stats, guess)
    end
  end
end

def compute_score(guess, code)
  mismatched = code.zip(guess).select {|c, g| c != g}
  red = CODE_SIZE - mismatched.size
  c2 = mismatched.map {|c, _| c}
  g2 = mismatched.map {|_, g| g}
  white = count_white(c2, g2)
  [red, white]
end

def count_white(code, guess)
  if guess.size == 0
    0
  else
    i = code.index(guess.first)
    if i
      code.delete_at(i)
      1 + count_white(code, guess[1..-1])
    else
      count_white(code, guess[1..-1])
    end
  end
end

# Given a code, compute the frequencies of the unique digits in the
# code and sort into reverse numerical order.  E.g., a code with all
# unique digits maps to [1,1,1,1], a code with two digits the same
# maps to [2,1,1], etc.
#
def categorize(code)
  code.group_by{|x| x}.values.map{|v| v.size}.sort.reverse
end

# uniq_by returns the first unique instance, uniq(&block) returns the last.
#
def uniq_by(list, &block)
  list.group_by(&block).values.map(&:first)
end

class Stats
  def initialize(solved = 0, total = 0, max_depth = 0)
    @solved = solved
    @total = total
    @max_depth = max_depth
  end

  def solved(depth)
    Stats.new(@solved + 1, @total + depth,
              depth > @max_depth ? depth : @max_depth)
  end

  def to_s
    average = @total.to_f / @solved
    "solved: #{@solved}, average: #{average}, maxDepth: #{@max_depth}"
  end
end
