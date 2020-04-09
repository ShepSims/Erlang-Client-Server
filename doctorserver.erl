%% File: doctorserver.erl
%% Author: Shep Sims
%% Purpose: provides a server node for the doctor program.

-module(doctorserver).

-export([start/0, connect/1, server_loop/0]).

%% Tells the terminal that the server loop has been started, and begins the server loop
start() -> 
	io:fwrite("Server started... \n"),
	register(doctorserver, spawn(doctorserver, server_loop, [])).

%% Connects clients to the RPC
connect(Name) -> rpc(Name).

%% takes a request and processes it, sending the request forward to the doctorserver
rpc(Request) ->
	doctorserver ! {self(), Request},
	receive
		{doctorserver, Response} ->
			Response
	end.

%% A server loop, which allows clients to connect, 
%% then dispatches them through to a client handler for interface
server_loop() ->
	receive
		{Pid, Request} ->
			io:fwrite("Client " ++ Request ++ " connecting . . .\n"),
			State = doctor:state(Request),
			Greeting = doctor:greeting(State),
			Pid ! {doctorserver, {Greeting, doctorclienthandler:start(Pid, State)}},
			server_loop()
	end.