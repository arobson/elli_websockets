# Web Sockets For Elli!
This was done during HackNashville with little sleep and lots of caffeine (it's good for you). There are things missing. This is pretend-ware, it will get better in short-ish order :)

## Use
To use this, you'll have to add the middleware to Elli and implement a websocket behavior.

### Add The Middleware
Here is an example of how I configure this along with the static file middleware in a supervisor:

```erlang
-module(frenetic_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 3,
	MaxSecondsBetweenRestarts = 60,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	FileserveConfig = [{prefix, <<"/public">>},
					   {path, <<"./public">>}],

	WSConfig = [{path, <<"/websocket">>},
				{websocket, my_websocket_behavior}],

	MWConfig = [{mods, [
			{elli_fileserve, FileserveConfig},
			{elli_websocket, WSConfig}
		]}],
	ElliOpts = [{callback, elli_middleware}, {port, 8090}, {callback_args,MWConfig}, {upgrade, true}],

	Elli = create_child_spec(elli, worker, permanent, 1000, [ElliOpts]),
	
	{ok, {SupFlags, [Elli]}}.

create_child_spec(Child, Type, Restart, Shutdown, Args) ->
	{Child, { Child, start_link, Args }, Restart, Shutdown, Type, [Child]}.
```

I hope you were looking closely. The {upgrade, true} in the ElliOpts is required for this to work even a little. Without the upgrade flag, Elli will shut any ws requests down.

### Create Your WebSocket Behavior
This is a silly behavior implementation. I suggest maybe making this also implement a gen_server or something that can track the sockets over time. You must provide an identifier for the socket which will be used to identify which socket you are receiving data/events from.

```erlang
%%% the saddest websocket behavior that ever was...
-module(my_websocket).
-behavior(websocket).
-export([handle_client/2, handle_close/1, handle_message/2, handle_ready/2]).

%% exists to provide a unique socket identifier based on request headers
handle_client(Headers) ->
	SendCallback("Hi, new websocket client!"),
	new_id. %% you want this to be unique per socket

%% called when the socket is closed for any reason
%% Reason :: socket_closed | client_signal | protocol_error
handle_close(Id, Reason) -> io:format("Websocket ~p closed. Reason: ~p.~n", [Id, Reason]).

%% handles incoming messages
handle_message(Id, Message) -> io:format("~p : ~p ~n", [Id, Message]).

%% once the socket is ready, you get a fun/1 for sending messages to the socket
handle_ready(Id, Send) -> Send("hello, world!").
```


## Install
Add the following to your rebar.config in the {deps,[]} collection:

```erlang
	{elli, "",
		{git, "git://github.com/arobson/elli",
		{branch, "upgrade" } } },
	{elli_websockets, "",
        {git, "git://github.com/arobson/elli_websockets",
        {branch, "master" } } }
```

Take note that you have to use my upgrade branch in my fork of elli to get this working for the moment. I may have to patch this middleware before it works with Elli on master.

## TO DO
	* normalize headers/allow for case-insensitive retrieval
	* trap and handle process exit signals from socket_loop
	* add support for hixie protocols
	* provide a better example websocket behavior
	* un-stupid this readme when I've slept
	* tests
	* take over the internets