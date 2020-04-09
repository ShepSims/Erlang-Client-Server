%% File: doctorclienthandler.erl
%% Author: Shep Sims
%% Purpose: provides a server client handler for the doctor program.

-module(doctorclienthandler).

-export([start/2, reply/2, client_loop/2]).

%% Spawns the client handler loop
start(Pid, State) -> spawn(doctorclienthandler, client_loop, [Pid, State]).

%% Sends the RPC the patients response
reply(Pid, PatientInput) -> rpc(Pid, PatientInput).

%% Sends the client handler loop the patient input
rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} ->
		Response
	end.

%% Processes the patient input, if it is the atom quit, then the server disconnects, 
%% else, the loop calls the doctor's reply function, and restarts its loop,
%% awaiting a reply from the patient
client_loop(Pid, State) ->
	receive
		{Pid, quit} ->
			Pid ! {self(), doctor:farewell(State)},
			io:fwrite("Client " ++ doctor:name(State) ++ " disconnected...\n");
		{Pid, PatientInput} ->
			Pid ! {self(), doctor:reply(PatientInput, State)},
            client_loop(Pid, doctor:add_sentence(PatientInput, State)),
            done
        end.