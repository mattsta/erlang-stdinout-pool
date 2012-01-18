-module(stdinout).

-export([start_link/2, start_link/4, start_link/5]).

-export([send/2, send/3]).
-export([reload/1]).
-export([pipe/2]).
-export([shutdown/1]).

-define(TIMEOUT, 60000).

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
%% respawn all running processes
%%====================================================================
reload(Server) ->
  gen_server:call(Server, reload, ?TIMEOUT).

%%====================================================================
%% stdin->stdout through pool or network
%%====================================================================
send({Host, Port}, Content) ->
  send(Host, Port, Content);
send(Server, Content) ->
  gen_server:call(Server, {stdin, Content}, ?TIMEOUT).

%%====================================================================
%% stdin->stdout through network
%%====================================================================
send(Host, Port, Content) ->
  case integer_to_list(iolist_size(Content)) of
       "0" -> [];   % if we aren't sending anything, don't send anything
    Length -> Formatted = [Length, "\n", Content],
              {ok, Sock} =
                gen_tcp:connect(Host, Port, [binary, {active, false}]),
              gen_tcp:send(Sock, Formatted),
              recv_loop(Sock, [])
  end.

recv_loop(Sock, Accum) ->
  case gen_tcp:recv(Sock, 0) of
          {ok, Bin} -> recv_loop(Sock, [Bin | Accum]);
    {error, closed} -> lists:reverse(Accum)
  end.

%%====================================================================
%% stdin->stdout through a series of pipes using pool or network
%%====================================================================
pipe(Content, []) ->
  Content;
% If ErrorRegex is an integer, we have a {Host, Port} tuple, not a regex.
pipe(Content, [{Server, ErrorRegex} | T]) when not is_integer(ErrorRegex) ->
  Stdout = send(Server, Content),
  case re:run(Stdout, ErrorRegex) of
       nomatch -> pipe(Stdout, T);
    {match, _} -> {error, Server, Stdout}
  end;
pipe(Content, [Server | T]) ->
  pipe(send(Server, Content), T).

%%===================================================================
%% Stopping
%%====================================================================
shutdown(Server) when is_pid(Server) ->
  exit(Server, shutdown);
shutdown(Server) when is_atom(Server) ->
  shutdown(whereis(Server)).
