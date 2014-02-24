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
-include_lib("elli/include/elli.hrl").

%% ==================================================================
%%  elli_handler
%% ==================================================================

init(Req, Config) ->
	Path = elli_request:raw_path(Req),
	Verb = elli_request:method(Req),
	CorrectPath = get_path(Config),
	case {Verb, Path} of
		{'GET', CorrectPath} -> {ok, handover};
		_ -> ignore
	end.

handle(Req, Config) ->
	WebSocket = proplists:get_value(websocket, Config),
	Headers = elli_request:headers(Req),
	Socket = Req#req.socket,
	shake(Headers, Socket, WebSocket).

%% this is just here to keep the Elli happy.
handle_event(_,_,_) -> ok.

%% ==================================================================
%%  Sockety Bits
%% ==================================================================
get_path(Config) ->
	proplists:get_value(path, Config, <<"/websocket">>).

%% perform the handshake, get the Id from the registered callback module
%% spawn a loop just for getting incoming messages and handling the socket
shake(Headers, Socket, WebSocket) ->
	Module = hybi_10_17,
	gen_tcp:send(Socket, Module:handshake(Headers)),
	inet:setopts(Socket,[{packet,0}]),
	SocketId = WebSocket:handle_client(Headers),
	Pid = spawn(fun() -> socket_loop(SocketId, Socket, Module, WebSocket) end),
	gen_tcp:controlling_process(Socket, Pid),
	WebSocket:handle_ready(SocketId, fun(Message) -> Pid ! {send, Message} end),
	Pid.

%% socket loooooooooooooooop!
socket_loop(SocketId, Socket, Module, WebSocket) ->
	inet:setopts(Socket,[{active,once}]),
	receive
		{send, Data} ->
			gen_tcp:send(Socket,Module:format_data(Data)),
			socket_loop(SocketId, Socket, Module, WebSocket);
		{tcp,Socket,Data} ->
			case Module:handle_data(Data) of
				{send, Message} -> gen_tcp:send(Socket, Message);
				websocket_close ->
					WebSocket:handle_close(SocketId, client_signal),
					gen_tcp:close(Socket);
				Result -> WebSocket:handle_message(SocketId, Result)
			end,
			socket_loop(SocketId, Socket, Module, WebSocket);
		{tcp_closed,Socket} -> 
			WebSocket:handle_close(SocketId, socket_closed),
			gen_tcp:close(Socket),
			ok;
		{websocket_close, Signal} ->
			WebSocket:handle_close(SocketId, protocol_error),
			gen_tcp:send(Socket, Signal),
			gen_tcp:close(Socket)
	end.