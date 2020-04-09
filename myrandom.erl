%% File: myrandom.erl

%% Purpose: For random integers using a monad.

-module(myrandom).

-export([test_random_int/2, test_probability/2, random_int/1, probability/1,
         pick_random/1]).


%% Returns a random number between 0 and n - 1, inclusive.
random_int(N) ->
  trunc(rand:uniform() * N).

%% Returns true ProbFactor of the time.
probability(ProbFactor) ->
    ProbRange = round(100 * ProbFactor),
    RandomValue = random_int(100),
    RandomValue > (100 - ProbRange).
    
%% Returns a random item from the list.
pick_random(List) -> lists:nth(random_int(length(List)) + 1, List).

%% Prints random integers between 0 and n - 1, for
%% a given number of iterations.

test_random_int(_N, 0) -> done;
test_random_int(N, Iterations) ->
        io:fwrite("~p~n", [random_int(N)]),
        test_random_int(N, Iterations - 1).

%% Prints True or False for a given probability, for
%% a given number of iterations.
test_probability(_Factor, 0) -> done;
test_probability(Factor, Iterations) ->
    io:fwrite("~p~n", [probability(Factor)]),
    test_probability(Factor, Iterations - 1).
