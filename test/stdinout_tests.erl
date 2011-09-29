-module(stdinout_tests).
-include_lib("eunit/include/eunit.hrl").

-define(E(A, B), ?assertEqual(A, B)).
-define(_E(A, B), ?_assertEqual(A, B)).
-define(B(X), iolist_to_binary(X)).

get_base_dir(Module) ->
  {file, Here} = code:is_loaded(Module),
  filename:dirname(filename:dirname(Here)).

add_oneshot_dep() ->
  code:add_path(get_base_dir(?MODULE) ++ "deps/oneshot/ebin").

setup_ports() ->
  add_oneshot_dep(),

  % Obviously this test only works if you have /bin/cat and /usr/bin/wc
  S1 = stdinout:start_link(cat1, "/bin/cat"),
  S2 = stdinout:start_link(cat2, "/bin/cat"),
  S3 = stdinout:start_link(cat3, "/bin/cat"),
  S4 = stdinout:start_link(cat4, "/bin/cat"),
  S5 = stdinout:start_link(wc,   "/usr/bin/wc"),

  S6 = stdinout:start_link(catn1, "/bin/cat",    "127.0.0.1", 6651),
  S7 = stdinout:start_link(catn2, "/bin/cat",    "127.0.0.1", 6652),
  S8 = stdinout:start_link(wcn,   "/usr/bin/wc", "127.0.0.1", 6653),

  [unlink(P) || {ok, P} <- [S1, S2, S3, S4, S5, S6, S7, S8]],
  [P || {ok, P} <- [S1, S2, S3, S4, S5, S6, S7, S8]].

cleanup_ports(Ps) ->
  [stdinout:shutdown(P) || P <- Ps].

everything_erlang_API_in_parallel_test_() ->
  {setup,
    fun setup_ports/0,
    fun cleanup_ports/1,
    fun(_) ->
      {inparallel,
        [
          ?_E(<<"hello">>, ?B(stdinout:send(cat1, "hello"))),
          ?_E(<<"hello">>, ?B(stdinout:send(cat1, <<"hello">>))),
          ?_E(<<"hello">>, ?B(stdinout:send(cat1, [<<"hello">>]))),
          ?_E(<<"hello">>, ?B(stdinout:send(cat1, [<<"he">>, <<"llo">>]))),
          ?_E(ok, stdinout:reload(cat1)),
          ?_E(<<"hello">>, ?B(stdinout:send(cat1, ["he", "llo"]))),
          ?_E([], stdinout:send(cat1, "")),
          ?_E(ok, stdinout:reload(cat2)),
          ?_E(<<"hello">>, ?B(stdinout:pipe("hello",[cat1, cat2, cat3, cat4]))),
          ?_E({error, cat1, <<"hello">>},
            % this wacky fun exists to erlang:iolist_to_binary/1 the ErrorIoList
            fun() ->
              {error, cat1, ErrorIoList} =
                stdinout:pipe("hello", [{cat1, "he"}, cat2, cat3, cat4]),
              {error, cat1, ?B(ErrorIoList)}
            end()),
          ?_E(ok, stdinout:reload(cat1)),
          ?_E({error, wc, <<"      0       1       5\n">>},
            fun() ->
              {error, wc, ErrorIoList} =
                stdinout:pipe("hello", [{cat1, "bob"}, {wc, "5"}, cat3, cat4]),
              {error, wc, ?B(ErrorIoList)}
            end())
        ]
      }
    end
  }.

-define(C1, {"localhost", 6651}).
-define(C2, {"localhost", 6652}).
-define(W1, {"localhost", 6653}).
network_API_test_() ->
  {setup,
    fun setup_ports/0,
    fun cleanup_ports/1,
    fun(_) ->
      {inparallel,
        [
          ?_E(<<"hello">>, ?B(stdinout:send(?C1, "hello"))),
          ?_E(<<"hello">>, ?B(stdinout:send(?C2, <<"hello">>))),
          ?_E(<<"hello">>, ?B(stdinout:send(?C1, [<<"hello">>]))),
          ?_E(<<"hello">>, ?B(stdinout:send(?C1, [<<"he">>, <<"llo">>]))),
          ?_E(<<"hello">>, ?B(stdinout:send(?C2, ["he", "llo"]))),
          ?_E([], stdinout:send(?C1, "")),
          ?_E(<<"hello">>, ?B(stdinout:pipe("hello", [?C1, ?C2, ?C1, ?C2]))),
          ?_E({error, ?C1, <<"hello">>},
            % this wacky fun exists to erlang:iolist_to_binary/1 the ErrorIoList
            fun() ->
              {error, ?C1, ErrorIoList} =
                stdinout:pipe("hello", [{?C1, "he"}, ?C2, ?C2, ?C1]),
              {error, ?C1, ?B(ErrorIoList)}
            end()),
          ?_E({error, ?W1, <<"      0       1       5\n">>},
            fun() ->
              {error, ?W1, ErrorIoList} =
                stdinout:pipe("hello", [{?C2, "bob"}, {?W1, "5"}, ?C2, ?C1]),
              {error, ?W1, ?B(ErrorIoList)}
            end())
        ]
      }
    end
  }.
