%% File: doctorclient.erl
%% Author: Shep Sims
%% Purpose: provides a terminal-based user interface for the doctor program.

-module(doctorclient).

-export([start/0]).

%% Prompts the user for his/her name, and connects him/her to the doctorserver, 
%% then prints the doctor's greeting
start() -> 
	Name = string:strip(io:get_line("Please enter your name: "), right, $\n),
	{Greeting, Clienthandlerid} = doctorserver:connect(Name),
	io:format(Greeting), 
	driver_loop(Clienthandlerid).

%% Takes input from the patient, and determines if the client continues to need assitance, 
%% or if he/she has asked to quit, if continued service is requested, restarts loop.
driver_loop(Clienthandlerid) -> 
	PatientInput = string:strip(io:get_line(">>> "), right, $\n),
	Exit = string:to_upper(PatientInput),		 
	if
   		Exit == "QUIT" ->
			io:format(doctorclienthandler:reply(Clienthandlerid, quit) ++ "\n");
		true ->
			io:format(doctorclienthandler:reply(Clienthandlerid, PatientInput) ++ "\n"),
            driver_loop(Clienthandlerid)
        end.