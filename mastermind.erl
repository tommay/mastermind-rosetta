-module(mastermind).
-export([main/0]).

%% Simple non-parallel implementation.
%% Run with:
%% erlc mastermind.erl
%% erl -noshell -run mastermind main -run init stop

-record(mastermind, {choices, code_size, all_codes}).
-define(is_mastermind(Term), is_record(Term, mastermind)).

-record(stats, {solved, total, max_depth}).
-define(is_stats(Term), is_record(Term, stats)).

new_mastermind() ->
    Choices = lists:seq(1, 5),
    CodeSize = 4,
    AllCodes = make_codes(Choices, CodeSize),
    #mastermind{
       choices = Choices,
       code_size = CodeSize,
       all_codes = AllCodes
      }.

%% Returns a new stats record.
%%
new_stats() ->
    #stats{
       solved = 0,
       total = 0,
       max_depth = 0
      }.

main() ->
    Mastermind = new_mastermind(),
    StartingGuesses = uniq_by(fun categorize/1,
			      Mastermind#mastermind.all_codes),
    Categories = lists:map(fun categorize/1, StartingGuesses),
    Stats = lists:map(
	      fun (StartingGuess) ->
		      compute_stats(Mastermind, StartingGuess)
	      end,
	      StartingGuesses),
    Zipped = lists:zip(Categories, Stats),
    lists:map(fun ({C, S}) ->
		      io:format("~p: ~s~n", [C, stats_to_string(S)])
	      end,
	      Zipped).

stats_to_string(Stats) ->
    Average = float(Stats#stats.total) / float(Stats#stats.solved),
    io_lib:format("Solved: ~p, average: ~p, max_depth: ~p",
		  [Stats#stats.solved, Average, Stats#stats.max_depth]).

compute_stats(Mastermind, StartingGuess) ->
    lists:foldl(
      fun (Code, Stats) ->
	      fold_code(Mastermind, StartingGuess, Code, Stats)
      end,
      new_stats(),
      Mastermind#mastermind.all_codes).

fold_code(Mastermind, StartingGuess, Code, Stats) ->
    fold_guess(Mastermind, Code, 1, Mastermind#mastermind.all_codes,
	       Stats, StartingGuess).

fold_guess(Mastermind, Code, Depth, Possibilities, Stats, Guess) ->
    Score = compute_score(Mastermind, Guess, Code),
    {Red, _} = Score,
    case Red == Mastermind#mastermind.code_size of
	true -> solved_it(Depth, Stats);
	false ->
	    RemainingPossibiltiies =
		lists:filter(
		  fun (P) ->
			  compute_score(Mastermind, Guess, P) == Score
		  end,
		  Possibilities),
	    lists:foldl(
	      fun (P, A) ->
		      fold_guess(Mastermind, Code, Depth + 1,
				 RemainingPossibiltiies, A, P)
	      end,
	      Stats,
	      RemainingPossibiltiies)
    end.

%% Given a guess and a code, return a pair containing the numnber of
%% exact matches (red), and the number of "right color wrong place"
%% (white).  The result is the same if guess and code are swapped.
%%
compute_score(Mastermind, Guess, Code) ->
    Mismatched = lists:filter(
		   fun ({G, C}) -> G /= C end,
		   lists:zip(Guess, Code)),
    Red = Mastermind#mastermind.code_size - length(Mismatched),
    {C2, G2} = lists:unzip(Mismatched),
    White = count_white(G2, C2),
    {Red, White}.

count_white([], _) -> 0;
count_white([G | Rest], Code) ->
  case remove(G, Code) of
      %% Using a single-element tuple as a Maybe.
      {} -> count_white(Rest, Code);
      {NewCode} -> 1 + count_white(Rest, NewCode)
  end.

%% remove first occurrence of the element from the list.  If the
%% element was not found returns {}, otherwise {list}.
%%
remove(_, []) -> {};
remove(Element, [First|Rest]) ->
    case Element == First of
	true -> {Rest};
	false ->
	    case remove(Element, Rest) of
		{} -> {};
		{Removed} -> {[First | Removed]}
	    end
    end.

make_codes(_, 0) -> [[]];
make_codes(Choices, Size) ->
    Codes = make_codes(Choices, Size - 1),
    lists:flatmap(
      fun (Choice) ->
	      lists:map(
		fun (Code) -> [Choice | Code] end,
		Codes)
      end,
      Choices).

%% Given a code, compute the frequencies of the unique digits in the
%% code and sort into reverse numerical order.  E.g., a code with all
%% unique digits maps to [1,1,1,1], a code with two digits the same
%% maps to [2,1,1], etc.
%%
categorize(Code) ->
    Counted = count(Code),
    Counts = maps:values(Counted),
    lists:reverse(lists:sort(Counts)).

solved_it(Depth, Stats) ->
    Stats#stats{
      solved = Stats#stats.solved + 1,
      total = Stats#stats.total + Depth,
      max_depth = max(Stats#stats.max_depth, Depth)
     }.

%% Takes a list of items, and returns a map where the keys are the
%% items and the values are the number of times the item occurs in the
%% list.
%%
count(List) ->
    lists:foldl(
      fun (E, M) ->
	      maps:update_with(E, fun (V) -> V + 1 end, 1, M)
      end,
      maps:new(),
      List).

%% Takes a list of items and a function that maps an item to a key,
%% and returns a list of unique items based on the key.
%%
uniq_by(Func, List) ->
    Map = lists:foldl(
	    fun (E, M) -> maps:put(Func(E), E, M) end,
	    maps:new(),
	    List),
    maps:values(Map).
