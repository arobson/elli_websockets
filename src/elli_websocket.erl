%%% @author Alex Robson
%%% @doc
%%%
%%% Middleware to handle incoming upgrade requests from client
%%% and sprinkle websocket dust on it.
%%% 
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created Oct 18, 2013 by Alex Robson
-module(elli_websocket).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).

%% ==================================================================
%%  API
%% ==================================================================

handle(Req, Config) ->
	Path = elli_request:raw_path(Req),
	Verb = elli_request:verb(Req),
	CorrectPath = get_path(Config),
	WebSocket = proplists:get_value(websocket, Config),
	case {Verb, Path} of
		{'GET', CorrectPath} ->
			{upgrade, fun(Headers, Body, Socket) ->
				shake(Headers, Body, Socket, WebSocket)
			end};
		_ -> ignore
	end.

%% this is just here to keep the Elli happy.
handle_event(_,_,_) -> ok.


%% ==================================================================
%%  Sockety Bits
%% ==================================================================
get_path(Config) ->
	proplists:get_value(path, Config, <<"websocket">>).

%% perform the handshake, get the Id from the registered callback module
%% spawn a loop just for getting incoming messages and handling the socket
shake(Headers, _, Socket, WebSocket) ->
	Module = hybi_10_17,
	gen_tcp:send(Socket, Module:handshake(Headers)),
	inet:setopts(Socket,[{packet,0}]),
	SocketId = WebSocket:handle_client(Headers),
	Pid = spawn(fun() -> socket_loop(SocketId, Socket, Module, WebSocket) end),
	gen_tcp:controlling_process(Socket, Pid),
	Pid.

%% socket loooooooooooooooop!
socket_loop(SocketId, Socket, Module, WebSocket) ->
	inet:setopts(Socket,[{active,once}]),
	receive
		{send, Data} ->
			gen_tcp:send(Socket,Module:send_format(Data)),
			socket_loop(SocketId, Socket, Module, WebSocket);
		{tcp,Socket,Data} ->
			case Module:handle_data(Data) of
				{send, Message} -> gen_tcp:send(Socket, Message);
				Result -> WebSocket:handle_message(SocketId, Result)
			end,
			socket_loop(SocketId, Socket, Module, WebSocket);
		{tcp_closed,Socket} -> 
			WebSocket:handle_close(SocketId, disconnected),
			ok;
		websocket_close ->
			WebSocket:handle_close(SocketId, closed),
			ok;
		{websocket_close, Signal} ->
			WebSocket:handle_close(SocketId, closed),
			gen_tcp:send(Socket, Signal)
	end.