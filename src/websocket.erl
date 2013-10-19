%%% @author Alex Robson
%%% @doc
%%%
%%% Web Socket Behavior
%%% 
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created Oct 19, 2013 by Alex Robson
-module(websocket).

-callback handle_client(Headers :: [tuple()], SendMessage :: fun((term(),term()) -> ok)) -> term().
-callback handle_message(SocketId :: term(), Message :: list()) -> ok.
-callback handle_close(SocketId :: term()) -> ok.