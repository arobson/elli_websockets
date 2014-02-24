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
-export([handle/2, handle_event/3, init/2]).
-include_lib("elli/include/elli.hrl").

%% ==================================================================
%%  elli_handler
%% ==================================================================

%% let elli know if we're going to be taking over the rest of the request
init(Req, Config) ->
	case is_websocket_request(Req, Config) of
		true -> {ok, handover};
		false -> ignore
	end.

%% if this is a web socket request, perform the handshake, otherwise ignore
handle(Req, Config) ->
	case is_websocket_request(Req, Config) of
		false -> ignore;
		_ ->
			WebSocket = proplists:get_value(websocket, Config),
			Headers = elli_request:headers(Req),
			Socket = Req#req.socket,
			shake(Headers, Socket, WebSocket)
	end.

%% presently, not doing anything with these ... mistake?
handle_event(_Name, _Event, _Args) -> ok.

is_websocket_request(Req, Config) ->
	Path = elli_request:raw_path(Req),
	Verb = elli_request:method(Req),
	SocketPath = get_path(Config),
	case {Verb, Path} of
		{'GET', SocketPath} -> true;
		_ -> false
	end.

%% ==================================================================
%%  Sockety Bits
%% ==================================================================
get_path(Config) ->
	proplists:get_value(path, Config, <<"/websocket">>).

%% set the socket options depending on socket type
set_socket_opts({plain, Raw}, Opts) -> inet:setopts(Raw, Opts);
set_socket_opts({ssl, Raw}, Opts) -> ssl:setopts(Raw, Opts).

%% perform the handshake, get the Id from the registered callback module
%% spawn a loop just for getting incoming messages and handling the socket
shake(Headers, Socket, WebSocket) ->
	Module = hybi_10_17,
	elli_tcp:send(Socket, Module:handshake(Headers)),
	set_socket_opts(Socket, [{packet,0}]),
	SocketId = WebSocket:handle_client(Headers),
	Pid = self(),
	WebSocket:handle_ready(SocketId, fun(Message) -> Pid ! {send, Message} end),
	socket_loop( SocketId, Socket, Module, WebSocket).

%% this socket loop handles all incoming and outgoing communication
%% for the socket once established.
socket_loop(SocketId, Socket, Module, WebSocket) ->
	set_socket_opts(Socket,[{active,once}]),
	receive
		{send, Data} ->
			elli_tcp:send(Socket,Module:format_data(Data)),
			socket_loop(SocketId, Socket, Module, WebSocket);
		{tcp,Socket,Data} ->
			case Module:handle_data(Data) of
				{send, Message} -> elli_tcp:send(Socket, Message);
				websocket_close ->
					WebSocket:handle_close(SocketId, client_signal),
					elli_tcp:close(Socket);
				Result -> WebSocket:handle_message(SocketId, Result)
			end,
			socket_loop(SocketId, Socket, Module, WebSocket);
		{tcp_closed,Socket} -> 
			WebSocket:handle_close(SocketId, socket_closed),
			elli_tcp:close(Socket),
			{close, <<>>};
		{websocket_close, Signal} ->
			WebSocket:handle_close(SocketId, protocol_error),
			elli_tcp:send(Socket, Signal),
			elli_tcp:close(Socket),
			{close, <<>>}
	end.