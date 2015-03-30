-module(stdinout_tests).
-include_lib("eunit/include/eunit.hrl").

-define(E(A, B), ?assertEqual(A, B)).
-define(_E(A, B), ?_assertEqual(A, B)).


get_base_dir(Module) ->
  {file, Here} = code:is_loaded(Module),
  filename:dirname(filename:dirname(Here)).

add_oneshot_dep() ->
  code:add_path(get_base_dir(?MODULE) ++ "deps/oneshot/ebin").

setup_ports() ->
  add_oneshot_dep(),

  Echoerr = get_base_dir(?MODULE) ++ "/priv/errcat",

  % Obviously this test only works if you have /bin/cat and /usr/bin/wc
  CAT1 = stdinout:start_link(cat1, "/bin/cat"),
  CAT2 = stdinout:start_link(cat2, "/bin/cat"),
  CAT3 = stdinout:start_link(cat3, "/bin/cat"),
  CAT4 = stdinout:start_link(cat4, "/bin/cat"),
  WC1  = stdinout:start_link(wc,   "/usr/bin/wc"),

  CAT6 = stdinout:start_link(catn1, "/bin/cat",    "127.0.0.1", 6651),
  CAT7 = stdinout:start_link(catn2, "/bin/cat",    "127.0.0.1", 6652),
  WC2  = stdinout:start_link(wcn,   "/usr/bin/wc", "127.0.0.1", 6653),

  ERR1 = stdinout:start_link(errcat,            Echoerr),
  ERR2 = stdinout:start_link(errcat_net,        Echoerr, "127.0.0.1", 6654),

  [unlink(P) || {ok, P} <- [CAT1, CAT2, CAT3, CAT4, WC1, CAT6, CAT7, WC2, ERR1, ERR2]],
  [P || {ok, P} <- [CAT1, CAT2, CAT3, CAT4, WC1, CAT6, CAT7, WC2, ERR1, ERR2]].

cleanup_ports(Ps) ->
  [stdinout:shutdown(P) || P <- Ps].

% Test responses on STDOUT
everything_erlang_API_in_parallel_test_() ->
  {setup,
    fun setup_ports/0,
    fun cleanup_ports/1,
    fun(_) ->
      {inparallel,
        [
          ?_E({ok, <<"hello">>},      stdinout:send(cat1, "hello")),
          ?_E({ok, <<"hello">>},      stdinout:send(cat1, <<"hello">>)),
          ?_E({ok, <<"hello">>},      stdinout:send(cat1, [<<"hello">>])),
          ?_E({ok, <<"hello">>},      stdinout:send(cat1, [<<"he">>, <<"llo">>])),
          ?_E(ok,                       stdinout:reload(cat1)),
          ?_E({ok, <<"hello">>},      stdinout:send(cat1, ["he", "llo"])),
          ?_E({ok, <<>>},                 stdinout:send(cat1, "")),
          ?_E(ok,                       stdinout:reload(cat2)),
          ?_E({ok, <<"hello">>},      stdinout:pipe("hello",[cat1, cat2, cat3, cat4])),

          ?_E({error, cat1, <<"hello">>},
                                        stdinout:pipe("hello", [{cat1, "he"}, cat2, cat3, cat4])),

          ?_E({error, wc, <<"      0       1       5\n">>},
                                        stdinout:pipe("hello", [{cat1, "bob"}, {wc, "5"}, cat3, cat4])),

          % Assert that binary responses on stdout can start with status byte 145 or 146
          ?_E({ok, <<145,23,88,97>>},      stdinout:send(cat1, <<145,23,88,97>>)),
          ?_E({ok, <<146,23,88,97>>},      stdinout:send(cat1, <<146,23,88,97>>)),
          ?_E({ok, <<146,23,88,97>>},      stdinout:pipe(<<146,23,88,97>>,[cat1, cat2, cat3, cat4]))
        ]
      }
    end
  }.

% Test responses on STDERR
everything_erlang_API_in_parallel_error_test_() ->
  {setup,
    fun setup_ports/0,
    fun cleanup_ports/1,
    fun(_) ->
      {inparallel,
        [
          ?_E({error, <<"hello">>},           stdinout:send(errcat, "hello")),
          ?_E({error, <<"hello">>},           stdinout:send(errcat, <<"hello">>)),
          ?_E({error, <<"hello">>},           stdinout:send(errcat, [<<"hello">>])),
          ?_E({error, <<"hello">>},           stdinout:send(errcat, [<<"he">>, <<"llo">>])),
          ?_E(ok,                               stdinout:reload(errcat)),
          ?_E({error, <<"hello">>},           stdinout:send(errcat, ["he", "llo"])),
          ?_E({error, errcat, <<"hello">>},   stdinout:pipe("hello",[errcat, cat2, cat3, cat4])),
          ?_E({error, errcat, <<"hello">>},   stdinout:pipe("hello",[cat1, cat2, errcat, cat4])),

          % Assert that binary responses on stderr can start with status byte 145 or 146
          ?_E({error, <<145,23,88,97>>},              stdinout:send(errcat, <<145,23,88,97>>)),
          ?_E({error, <<146,23,88,97>>},              stdinout:send(errcat, <<146,23,88,97>>)),
          ?_E({error, errcat, <<146,23,88,97>>},      stdinout:pipe(<<146,23,88,97>>,[errcat, cat2, cat3, cat4])),
          ?_E({error, errcat, <<146,23,88,97>>},      stdinout:pipe(<<146,23,88,97>>,[cat1, cat2, errcat, cat4]))
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
          ?_E({ok, <<"hello">>},      stdinout:send(?C1, "hello")),
          ?_E({ok, <<"hello">>},      stdinout:send(?C2, <<"hello">>)),
          ?_E({ok, <<"hello">>},      stdinout:send(?C1, [<<"hello">>])),
          ?_E({ok, <<"hello">>},      stdinout:send(?C1, [<<"he">>, <<"llo">>])),
          ?_E({ok, <<"hello">>},      stdinout:send(?C2, ["he", "llo"])),
          ?_E({ok, <<>>},                 stdinout:send(?C1, "")),
          ?_E({ok, <<"hello">>},      stdinout:pipe("hello", [?C1, ?C2, ?C1, ?C2])),

          ?_E({error, ?C1, <<"hello">>},
                                        stdinout:pipe("hello", [{?C1, "he"}, ?C2, ?C2, ?C1])),

          ?_E({error, ?W1, <<"      0       1       5\n">>},
                                        stdinout:pipe("hello", [{?C2, "bob"}, {?W1, "5"}, ?C2, ?C1])),

          % Assert that network binary responses on stdout can start with status byte 145 or 146
          ?_E({ok, <<145,23,88,97>>},      stdinout:send(?C1, <<145,23,88,97>>)),
          ?_E({ok, <<146,23,88,97>>},      stdinout:send(?C2, <<146,23,88,97>>)),
          ?_E({ok, <<146,23,88,97>>},      stdinout:pipe(<<146,23,88,97>>, [?C1, ?C2, ?C1, ?C2]))
        ]
      }
    end
  }.

-define(E2, {"localhost", 6654}).
network_API_error_test_() ->
  {setup,
    fun setup_ports/0,
    fun cleanup_ports/1,
    fun(_) ->
      {inparallel,
        [
          ?_E({error, <<"hello">>},           stdinout:send(?E2, "hello")),
          ?_E({error, <<"hello">>},           stdinout:send(?E2, <<"hello">>)),
          ?_E({error, <<"hello">>},           stdinout:send(?E2, [<<"hello">>])),
          ?_E({error, <<"hello">>},           stdinout:send(?E2, [<<"he">>, <<"llo">>])),
          ?_E({error, <<"hello">>},           stdinout:send(?E2, ["he", "llo"])),

          ?_E({error,{"localhost",6654},<<"hello">>},
                                                stdinout:pipe("hello", [?C1, ?C2, ?E2, ?C1])),

          % Assert that network binary responses on stderr can start with status byte 145 or 146
          ?_E({error, <<145,23,88,97>>},                      stdinout:send(errcat, <<145,23,88,97>>)),
          ?_E({error, <<146,23,88,97>>},                      stdinout:send(errcat, <<146,23,88,97>>)),
          ?_E({error, {"localhost",6654}, <<146,23,88,97>>},  stdinout:pipe(<<146,23,88,97>>,[?C1, ?C2, ?E2, ?C1]))
        ]
      }
    end
  }.
