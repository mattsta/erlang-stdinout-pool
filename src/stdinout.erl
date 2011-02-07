-module(stdinout).

%% gen_server callbacks
-compile(export_all).

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

%%====================================================================
%% Stopping
%%====================================================================
shutdown(Server) ->
  Server ! shutdown.
