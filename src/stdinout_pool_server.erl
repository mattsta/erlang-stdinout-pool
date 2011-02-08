-module(stdinout_pool_server).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/1, start_link/2, start_link/4, start_link/5]).

% oneshot callback
-export([handle_oneshot/1]).

-record(state, {cmd, ip, port, available, reserved, count, forcer}).

%%====================================================================
%% api callbacks
%%====================================================================
start_link(Cmd) ->
  start_link(?MODULE, Cmd).

start_link(GenServerName, Cmd) ->
  start_link(GenServerName, Cmd, none, none).

start_link(GenServerName, Cmd, IP, Port) ->
  start_link(GenServerName, Cmd, IP, Port, count_cpus()).

start_link(GenServerName, Cmd, IP, Port, SocketCount) ->
  gen_server:start_link({local, GenServerName}, ?MODULE,
    [Cmd, IP, Port, SocketCount], []).

count_cpus() ->
  count_cpus(erlang:system_info(cpu_topology), 0).
count_cpus([], Count) ->
  Count;
count_cpus([{node, [{processor, Cores}]} | T], Count) ->
  count_cpus(T, Count + length(Cores));
count_cpus([{processor, Cores} | T], Count) ->
  count_cpus(T, Count + length(Cores)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Cmd, IP, Port, SocketCount]) ->
  process_flag(trap_exit, true),
  Forcer = get_base_dir(?MODULE) ++ "/priv/stdin_forcer",
  initial_setup(#state{cmd = Cmd, forcer = Forcer,
                       ip = IP, port = Port, count = SocketCount}).

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
% If we run out of available processes, just make another one.
% There should be a limit here. Maybe track used ones and make max 2xCount ports
% Or, we could make a waiting_clients list and service them on respawn
handle_call({stdin, Content}, From, #state{available = []} = State) ->
  NewAvail = [setup(State)],
  handle_call({stdin, Content}, From, State#state{available = NewAvail});

handle_call({stdin, Content}, From, #state{available = [H|T]} = State) ->
  % quickly spawn so we can be a non-blocking gen_server:
  spawn(fun() ->
          port_connect(H, self()),   % attach port to this spawned process
          port_command(H, Content),  % send our stdin content to the wrapper
          port_command(H, <<0>>),    % tell the wrapper we're done
          gen_server:reply(From, gather_response(H)),
          port_close(H)
        end),
  {noreply, State#state{available = T}};
 
handle_call(shutdown, _From, State) ->
  {stop, normal, State}.

gather_response(Port) ->
  gather_response(Port, []).
gather_response(Port, Accum) ->
  receive
    {Port, {data, Bin}} -> gather_response(Port, [Bin | Accum]);
    {Port, eof} -> lists:reverse(Accum)
  after % max 30 seconds of time for the process to send EOF (close stdout)
    30000 -> {died, lists:reverse(Accum)}
  end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

% A cmd exited.  Let's remove it from the proper list of servers.
handle_info({'EXIT', _Pid, _Reason},
    #state{available=Available, count = Count} = State)
      when length(Available) >= Count ->
    {noreply, State}; % don't respawn if we have more than Count live processes
handle_info({'EXIT', Pid, _Reason},
    #state{available=Available, reserved=Reserved} = State) ->
  case lists:member(Pid, Available) of
    true -> RemovedOld = Available -- [Pid],
            NewAvail = [setup(State) | RemovedOld],
            {noreply, State#state{available=NewAvail}};
    false -> RemovedOld = Reserved -- [Pid],
             NewAvail = [setup(State) | Available],
             {noreply, State#state{available = NewAvail, reserved = RemovedOld}}
  end;

handle_info(Info, State) ->
  error_logger:error_msg("Other info: ~p with state ~p~n", [Info, State]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{available = A, reserved = R}) ->
  [port_close(P) || P <- A],
  [port_close(P) || P <- R],
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_setup(#state{count = Count} = State) ->
  OpenedPorts =  [setup(State) || _ <- lists:seq(1, Count)],
  setup_external_server(State),
  {ok, State#state{
         available = OpenedPorts,
         reserved = []}}.

setup(#state{cmd = Cmd, forcer = Forcer}) ->
%  io:format("Opening ~s~n", [Cmd]),  % Uncomment to see re-spawns happen live
  open_port({spawn_executable, Forcer},
             [stream, use_stdio, stderr_to_stdout, binary, eof,
              {args, string:tokens(Cmd, " ")}]).

get_base_dir(Module) ->
  {file, Here} = code:is_loaded(Module),
  filename:dirname(filename:dirname(Here)).

%%--------------------------------------------------------------------
%%% oneshot TCP server callbacks/functions/executors
%%% The exit calls below exit the connected TCP process, not pool_server
%%--------------------------------------------------------------------

% Break if the first line received (the Length header) is > than 9 characters
oneshot_length(Sock, Acc) when length(Acc) > 9 ->
  gen_tcp:send(Sock, oneshot_error({length, lists:reverse(Acc)})),
  exit(normal);
oneshot_length(Sock, Acc) ->
  % Receive one byte at a time so we can stop at \n
  % (also protection against naughty clients who don't send length headers)
  case gen_tcp:recv(Sock, 1) of
    {ok, <<"\n">>} -> RAcc = lists:reverse(Acc),
                      try
                        list_to_integer(binary_to_list(iolist_to_binary(RAcc)))
                      catch
                        error:badarg ->
                          gen_tcp:send(Sock, oneshot_error({format, RAcc})),
                          exit(normal)
                      end;
    {ok, Bin}      -> oneshot_length(Sock, [Bin | Acc])
  end.

handle_oneshot(Pool) ->
  handle_oneshot(Pool, -1, []).
handle_oneshot(Pool, TotalSz, Acc) ->
  receive
    {socket_ready, Socket} -> % oneshot_length reads byte-by-byte.  no active.
                              inet:setopts(Socket, [{active, false}]),
                              TotalSize = oneshot_length(Socket, []),
                              % re-enable active so we receive messages here
                              inet:setopts(Socket, [{active, once}]),
                              handle_oneshot(Pool, TotalSize, Acc);
    {tcp, Socket, Bin} -> NewAcc = [Bin | Acc],
                          NewSz = iolist_size(NewAcc),
                          if
                            NewSz >= TotalSz ->
                              RAcc = lists:reverse(NewAcc),
                              % If we read more than TotalSz, only send up to
                              % TotalSz to stdinout:send/2.
                              UseAcc = case NewSz of
                                         TotalSz -> RAcc;
                                               _ -> UR = iolist_to_binary(RAcc),
                                                    <<Usable:TotalSz/bytes,
                                                      _/binary>> = UR,
                                                    Usable
                                        end,
                              Stdout = case stdinout:send(Pool, UseAcc) of
                                         {died, StdAccum} ->
                                           oneshot_error({died, StdAccum});
                                                    Value -> Value
                                        end,
                              ok = gen_tcp:send(Socket, Stdout);
                            true ->
                              inet:setopts(Socket, [{active, once}]),
                              handle_oneshot(Pool, TotalSz, NewAcc)
                          end
  after % max 30 seconds of socket wait time
    30000 -> died
  end.

oneshot_error(Error) ->
  [<<"STDINOUT_POOL_ERROR: ">>,
   oneshot_error_pretty(Error)].

oneshot_error_pretty({died, Accum}) ->
  [<<"Timeout reached when running command or no EOF received. ">>,
   <<"Data so far: ">>, Accum];
oneshot_error_pretty({format, Accum}) ->
  [<<"Non-number found in Length line: [">>, Accum, <<"].\n">>];
oneshot_error_pretty({length, Accum}) ->
  [<<"Length line too long: [">>, Accum, <<"] (first ten bytes).\n">>].


setup_external_server(#state{ip = none, port = none}) ->
  ok;
setup_external_server(#state{ip = IP, port = Port}) ->
  ThisGenServer = self(),
  {ok, _Pid} = 
  oneshot_server:start_link(IP, Port, fun() -> 
                                        ?MODULE:handle_oneshot(ThisGenServer)
                                       end).
