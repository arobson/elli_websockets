%%% @author Alex Robson
%%% @doc
%%%
%%% web socket handle for hybi 10 and 17: http://tools.ietf.org/html/rfc6455
%%%
%%%	This module may be a rather poor implementation but has a rather rich heritage.
%%% Please see the list of gifted developers that have made it possible.
%%%
%%% Structure and implementation have been taken from (in order of influence):
%%%	    MISULTIN - BSD License
%%%			https://github.com/ostinelli/misultin/blob/master/src/misultin_websocket_draft-hybi-10_17.erl
%%% 		Copyright 2011 - Richard Jones <rj@metabrew.com>, Roberto Ostinelli <roberto@ostinelli.net>
%%% 	Erl_Websocket Apache 2.0 License
%%%			https://github.com/awsong/erl_websocket
%%%			Copyright 2011 - Andy W. Song
%%%		and Joe Armstrong (Copyright? 2009)
%%%			http://armstrongonsoftware.blogspot.com/2009/12/comet-is-dead-long-live-websockets.html
%%% 
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created Oct 18, 2013 by Alex Robson

-module(hybi_10_17).

-export([handshake/1, handle_data/1, send/1]).

-record(state, {
	buffer	= <<>>,
	mask_key  = <<0,0,0,0>>,
	fragments = [] %% if we are in the midst of receving a fragmented message, fragments are contained here in reverse order
}).

-record(frame, {fin,
				rsv1,
				rsv2,
				rsv3,
				opcode,
				maskbit,
				length,
				maskkey,
				data}).

-define(OP_CONT, 0).
-define(OP_TEXT, 1).
-define(OP_BIN, 2).
-define(OP_CLOSE, 8).
-define(OP_PING, 9).
-define(OP_PONG, 10).

-define(IS_CONTROL_OPCODE(X), ((X band 8)=:=8) ).

%% If we don't find a websocket protocol frame in this many bytes, connection aborts
-define(MAX_UNPARSED_BUFFER_SIZE, 1024 * 100).


%% ==================================================================
%%  API
%% ==================================================================

%% Description: Callback to build handshake data.
handshake(Headers) ->
	% build data
	Key = proplists:get_value(<<"Sec-Websocket-Key">>, Headers),
	Accept = base64:encode_to_string(crypto:hash(sha, <<Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)),
	["HTTP/1.1 101 Switching Protocols\r\n",
		"Upgrade: websocket\r\n",
		"Connection: Upgrade\r\n",
		"Sec-WebSocket-Accept: ", Accept, "\r\n\r\n"
	].

%% Description: unmask client data
handle_data(Data) -> handle_data(Data, undefined).
handle_data(Data, _State) when is_list(Data) ->
	handle_data(list_to_binary(Data), undefined);
handle_data(Data, undefined) when is_binary(Data) ->
	% init status
	i_handle_data(#state{buffer = Data});
handle_data(Data, State = #state{buffer = Buffer}) when is_binary(Data) ->
	% read status
	i_handle_data(State#state{buffer = <<Buffer/binary, Data/binary>>}).

%% Description: Callback to format data before it is sent into the socket.
send(Data) ->
	send(Data, ?OP_TEXT).
send(Data, OpCode) ->
	BData = erlang:iolist_to_binary(Data),
	Len = erlang:size(BData),
	if
		Len < 126 ->
			<<1:1, 0:3, OpCode:4, 0:1, Len:7, BData/binary>>;
		Len < 65536 ->
			<<1:1, 0:3, OpCode:4, 0:1, 126:7, Len:16, BData/binary>>;
		true ->
			<<1:1, 0:3, OpCode:4, 0:1, 127:7, 0:1, Len:63, BData/binary>>
	end.

%% ==================================================================
%% Client Frame Protocol
%%	   0				   1				   2				   3
%%	   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%%	  +-+-+-+-+-------+-+-------------+-------------------------------+
%%	  |F|R|R|R| opcode|M| Payload len |	   Extended payload length	  |
%%	  |I|S|S|S|	 (4)  |A|	  (7)	  |				(16/63)			  |
%%	  |N|V|V|V|		  |S|			  |	  (if payload len==126/127)	  |
%%	  | |1|2|3|		  |K|			  |								  |
%%	  +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
%%	  |		Extended payload length continued, if payload len == 127  |
%%	  + - - - - - - - - - - - - - - - +-------------------------------+
%%	  |								  |Masking-key, if MASK set to 1  |
%%	  +-------------------------------+-------------------------------+
%%	  | Masking-key (continued)		  |			 Payload Data		  |
%%	  +-------------------------------- - - - - - - - - - - - - - - - +
%%	  :						Payload Data continued ...				  :
%%	  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
%%	  |						Payload Data continued ...				  |
%%	  +---------------------------------------------------------------+
%% ==================================================================

%% ==================================================================
%%  Frame Parsing
%% ==================================================================

%% parse received data and get the frames
-spec take_frame(Data::binary()) -> {#frame{} | undefined, Rest::binary()} | {error, max_size_reached}.
%% normal length
take_frame(<<Fin:1, 
			 Rsv1:1, %% Rsv1 = 0
			 Rsv2:1, %% Rsv2 = 0
			 Rsv3:1, %% Rsv3 = 0
			 Opcode:4,
			 MaskBit:1, %% must be 1
			 PayloadLen:7,
			 MaskKey:4/binary,
			 PayloadData:PayloadLen/binary-unit:8,
			 Rest/binary>>) when PayloadLen < 126 ->
	%% Don't auto-unmask control frames
	Data = case ?IS_CONTROL_OPCODE(Opcode) of
		true  -> PayloadData;
		false -> unmask(MaskKey,PayloadData)
	end,
	{#frame{fin=Fin, 
			rsv1=Rsv1,
			rsv2=Rsv2,
			rsv3=Rsv3,
			opcode=Opcode,
			maskbit=MaskBit,
			length=PayloadLen,
			maskkey=MaskKey,
			data = Data}, Rest};
%% extended payload (126)
take_frame(<<Fin:1, 
			 Rsv1:1, %% Rsv1 = 0
			 Rsv2:1, %% Rsv2 = 0
			 Rsv3:1, %% Rsv3 = 0
			 Opcode:4,
			 MaskBit:1, %% must be 1
			 126:7,
			 PayloadLen:16,
			 MaskKey:4/binary,
			 PayloadData:PayloadLen/binary-unit:8,
			 Rest/binary>>) ->
	{#frame{fin=Fin, 
			rsv1=Rsv1,
			rsv2=Rsv2,
			rsv3=Rsv3,
			opcode=Opcode,
			maskbit=MaskBit,
			length=PayloadLen,
			maskkey=MaskKey,
			data=unmask(MaskKey,PayloadData)},	Rest};
%% extended payload (127)
take_frame(<<Fin:1, 
			 Rsv1:1, %% Rsv1 = 0
			 Rsv2:1, %% Rsv2 = 0
			 Rsv3:1, %% Rsv3 = 0
			 Opcode:4,
			 MaskBit:1, %% must be 1
			 127:7, %% "If 127, the following 8 bytes interpreted as a 64-bit unsigned integer (the most significant bit MUST be 0)" 
			 0:1,	%% MSB of 0
			 PayloadLen:63,
			 MaskKey:4/binary,
			 PayloadData:PayloadLen/binary-unit:8,
			 Rest/binary>>) ->
	{#frame{fin=Fin, 
			rsv1=Rsv1,
			rsv2=Rsv2,
			rsv3=Rsv3,
			opcode=Opcode,
			maskbit=MaskBit,
			length=PayloadLen,
			maskkey=MaskKey,
			data=unmask(MaskKey, PayloadData)},	 Rest};

%% incomplete frame
take_frame(Data) when is_binary(Data), size(Data) < ?MAX_UNPARSED_BUFFER_SIZE ->
	{undefined, Data};

%% DOS guard
take_frame(Data) when is_binary(Data), size(Data) >= ?MAX_UNPARSED_BUFFER_SIZE ->
	{error, max_size_reached}.


i_handle_data(#state{buffer=ToParse} = State) ->
	case take_frame(ToParse) of
		{error, max_size_reached} ->
			%% faild to find frame within data limit
			{websocket_close, websocket_close_data()};
		{undefined, [_, <<0,0,0,0>>, []} ->
			%% client is failing to mask data, this is a protocol violation :@
			{websocket_close, websocket_close_data()};
		{undefined, Rest} ->
			%% no full frame to be had yet
			State#state{buffer = Rest};
		{Frame=#frame{}, Rest} ->
			%% sanity check, in case client is broken
			case sanity_check(Frame) of
				true ->
					case handle_frame(Frame, State#state{buffer = Rest}) of
						%% tail-call if there is stuff in the buffer still to parse
						NewState = #state{buffer = B} when is_binary(B), B =/= <<>> ->
							i_handle_data(NewState);
						Other ->
							Other
					end;
				false ->
					%% protocol violation, shut it down
					{websocket_close, websocket_close_data()}
			end
	end.

%% check to ensure the frame is formatted according to spec
sanity_check(Frame) ->
	Checks = [
		{1, Frame#frame.maskbit},
		{0, Frame#frame.rsv1},
		{0, Frame#frame.rsv2},
		{0, Frame#frame.rsv3}
	],
	lists:foldl(fun({A,B}, Acc) -> Acc andalso (A =:= B) end, true, Checks).


%% ==================================================================
%%  Fragment Handling
%% ==================================================================

%% FRAGMENT - add to the list and carry on
%% "A fragmented message consists of a single frame with the FIN bit
%%	clear and an opcode other than 0, followed by zero or more frames
%%	with the FIN bit clear and the opcode set to 0, and terminated by
%%	a single frame with the FIN bit set and an opcode of 0"
handle_frame(#frame{fin = 0, opcode = Opcode},
			 State = #state{fragments = []} = Frame) when Opcode =/= ?OP_CONT ->
	%% first fragment
	State#state{fragments = [Frame]};
handle_frame(#frame{fin = 0, opcode = ?OP_CONT}, %% subsequent fragments
			 State = #state{fragments = Frags} = Frame) when Frags =/= [] ->
	%% next fragement
	State#state{fragments = [Frame | Frags]};

%% Last frame in a fragmented message.
%% reassemble one large frame based on all the fragments, keeping opcode from first:
handle_frame(#frame{fin = 1, opcode = ?OP_CONT } = F, 
			 State = #state{fragments = Frags}) when Frags =/= [] ->
	[Frame1|Frames] = lists:reverse([F|Frags]),
	Frame = lists:foldl(
		fun(#frame{length = L, data = D}, AccF) ->
			%% NB: we unmask data as we parse frames, so concating here is ok:
			AccF#frame{length = (AccF#frame.length + L), data = << (AccF#frame.data)/binary, D/binary >>}
		end,
		Frame1#frame{fin=1},
		Frames
	),
	%% now process this new combined message as if we got it all at once:
	handle_frame(Frame, State#state{fragments = []});

%% end of fragments but no fragments stored - error
handle_frame(#frame{fin = 1, opcode = ?OP_CONT}, _) ->
	%% closing, should only happen if client is broken
	{websocket_close, websocket_close_data()};


%% ==================================================================
%%  Frame Handling (non-fragmented)
%% ==================================================================

%% CONTROL FRAMES: 	1) cannot be fragmented, thus have size <= 125bytes
%%				 	2) have an opcode where MSB is set
%%				 	3) can appear between larger fragmented message frames
handle_frame(#frame{fin=1, opcode=Opcode, data=Data},  _State) 
	when ?IS_CONTROL_OPCODE(Opcode) ->
	%% handle all known control opcodes:
	case Opcode of
		?OP_PING ->
			{send, send(Data, ?OP_PONG)};
		?OP_CLOSE ->
			%% websocket close requested
			websocket_close;
		_OpOther ->
			%% unknown opscode from client. shut it down.
			{websocket_close, websocket_close_data()}
	end;

%% NORMAL FRAME (not a fragment, not a control frame)
handle_frame(#frame{fin=1, opcode=Opcode, data=Data}, _) 
	when Opcode =:= ?OP_BIN; Opcode =:= ?OP_TEXT ->
	binary_to_list(Data).


unmask(Key, <<_:512,_Rest/binary>> = Data) ->
	K = binary:copy(Key, 512 div 32),
	<<LongKey:512>> = K,
	<<ShortKey:32>> = Key,
	unmask(ShortKey, LongKey, Data, <<>>);
unmask(Key, Data) ->
	<<ShortKey:32>> = Key,
	unmask(ShortKey,none, Data, <<>>).
unmask(Key, LongKey, Data, Accu) ->
	case Data of
		<<A:512, Rest/binary>> ->
			C = A bxor LongKey,
			unmask(Key, LongKey, Rest, <<Accu/binary, C:512>>);
		<<A:32,Rest/binary>> ->
			C = A bxor Key,
			unmask(Key, LongKey, Rest, <<Accu/binary, C:32>>);
		<<A:24>> ->
			<<B:24, _:8>> = <<Key:32>>,
			C = A bxor B,
			<<Accu/binary, C:24>>;
		<<A:16>> ->
			<<B:16, _:16>> = <<Key:32>>,
			C = A bxor B,
			<<Accu/binary, C:16>>;
		<<A:8>> ->
			<<B:8, _:24>> = <<Key:32>>,
			C = A bxor B,
			<<Accu/binary, C:8>>;
		<<>> ->
			Accu
	end.

websocket_close_data() -> <<1:1, 0:3, ?OP_CLOSE:4, 0:1, 0:7>>.