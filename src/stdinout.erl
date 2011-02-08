-module(stdinout).

-export([start_link/2, start_link/4, start_link/5]).

-export([send/2, send/3]).
-export([shutdown/1]).
%%====================================================================
%% Starting
%%====================================================================
start_link(GenServerName, Cmd) ->
  stdinout_pool_server:start_link(GenServerName, Cmd).

start_link(GenServerName, Cmd, IP, Port) ->
  stdinout_pool_server:start_link(GenServerName, Cmd, IP, Port).

start_link(GenServerName, Cmd, IP, Port, SocketCount) ->
  stdinout_pool_server:start_link(GenServerName, Cmd, IP, Port, SocketCount).

%%====================================================================
%% stdin->stdout
%%====================================================================
send(Server, Content) ->
  gen_server:call(Server, {stdin, Content}).

send(Host, Port, Content) ->
  {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
  Length = integer_to_list(iolist_size(Content)),
  Formatted = [Length, "\n", Content],
  gen_tcp:send(Sock, Formatted),
  recv_loop(Sock, []).

recv_loop(Sock, Accum) ->
  case gen_tcp:recv(Sock, 0) of
          {ok, Bin} -> recv_loop(Sock, [Bin | Accum]);
    {error, closed} -> lists:reverse(Accum)
  end.

%%===================================================================
%% Stopping
%%====================================================================
shutdown(Server) ->
  gen_server:call(Server, shutdown).
