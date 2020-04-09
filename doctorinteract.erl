%% File: doctorinteract.erl
%% Author: Ken Lambert
%% Purpose: provides a terminal-based user interface for the doctor program.

-module(doctorinteract).

-export([start/0]).
-import(doctor, [state/1, greeting/1, farewell/1, reply/2, add_sentence/2]).
-import(string, [to_upper/1, strip/3]).

start() ->
    Name = strip(io:get_line("Please enter your name: "), right, $\n),
    State = state(Name),
    io:format(greeting(State)),
    driver_loop(State).

driver_loop(State) ->
    PatientInput = strip(io:get_line(">>> "), right, $\n),
    Exit = to_upper(PatientInput),
    if
        Exit == "QUIT" ->
            io:format(farewell(State)),
            done;
        true -> io:format(reply(PatientInput, State) ++ "\n"),
                driver_loop(add_sentence(PatientInput, State))
        end.
