stdinout_pool: send to stdin and read stdout on external processes
==================================================================

[![Build Status](https://secure.travis-ci.org/mattsta/erlang-stdinout-pool.png)](http://travis-ci.org/mattsta/erlang-stdinout-pool)

What is it?
-----------
stdinout_pool maintains a pool of idle processes waiting for input.

You send input to a pool with `stdinout:send(Pool, Data)` and get back whatever
the process returns on stdout or stderr (as of version 2.0+, stdout and stderr
are identified independently).

You can also send input to a TCP port and get back the stdout output from a
process in your pool.

Why is this special?
--------------------
Erlang ports are unable to send EOF or close stdin on ports.  To get around
that limitation, stdinout_pool includes a C port to intercept stdin and
forward it to a spawned process.  When the C port sees a null byte
(automatically appended to your input when you call `stdinout:send/2`), it
closes stdin and sends stdout from the spawned process to normal stdout.

Erlang ports are also unable to differentiate between stdout and stderr.
To get round that limitation, starting with version 2.0.0,
stdinout_pool automatically tags stdout/stderr
for you and returns your output as an iolist wrapped
in an appropriate tuple: `{stdout, iolist()}` or `{stderr, iolist()}`.

Note: Your input must not contain null bytes or your input will terminate early.
There are no other restrictions on the input.  stdinout_pool was designed for
sending text between processes, so if you have more complex binary protocol needs,
you may need to modify the library to add a prefix length port protocol instead of
automatically relying on null byte markers to terminate input.

API
---
**BREAKING CHANGE**: Version 2.0+ returns output wrapped in `stdout` and `stderr`
tuples instead of returning a bare result.  If you don't care about stdout-vs-stderr,
you can use `stdinoutpool:unwrap/1` or just strip off the first element of the
result tuple yourself.  If the new output breaks your existing usage, you can
always the older verson by pulling the [v1.3.7](https://github.com/mattsta/erlang-stdinout-pool/releases/tag/v1.3.7) tag

---

All you need for `stdinout_pool` is a command to run and sit idle waiting for
stdin to close or get an eof.

`stdinout:start_link(Name, CommandWithArgs, [IP, Port, [NumberOfProcesses]])`
IP is a string IP (e.g. "127.0.0.1") or a tuple IP (e.g. {127, 0, 0, 1}).
Port is an integer.

`stdinout:send(ServerNameOrPid, Data)` returns an iolist of stdout from the pool
wrapped in a tuple of `{stdout, iolist()}` or `{stderr, iolist()}`.

The number of default idle processes is the number of cores the erlang VM sees
as determined by one of: `erlang:system_info(cpu_topology)`, `erlang:system_info(logical_processors_available)`,
or `erlang:system_info(schedulers_online)`.

Note: there is no upper limit on the number of spawned processes.
The process count is how
many already-spun-up-and-waiting processes to keep lingering.  If you have 4
waiting processes but suddenly get 30 requests, extra processes will be spawned
to accomidate your workload (and your throughput will take a dive because you
will be limited by process spawn time).  When everything settles down again, you will be back to 4 idle processes.

You can optionally pass in an IP and Port combination to create a network
server to respond to requests.  If IP and Port are each set to the atom `none`,
then no network server is created.

Usage
-----
### Erlang API Basic STDIN/STDOUT Usage (updated for 2.0+)

```erlang
Eshell V6.0  (abort with ^G)
1> stdinout:start_link(uglify, "/home/matt/bin/cl-uglify-js").
{ok,<0.34.0>}
2> stdinout:send(uglify, "(function() { function YoLetsReturnThree(){ return 3 ; } function LOLCallingThree() { return YoLetsReturnThree() ; }; LOLCallingThree();})();").
{stdout,[<<"(function(){function b(){return a()}function a(){return 3}b()})();">>]}
3> stdinout:start_link(closure, "/usr/java/latest/bin/java -jar /home/matt/bin/closure/compiler.jar").
{ok,<0.38.0>}
4> stdinout:send(closure, "(function() { function YoLetsReturnThree(){ return 3 ; } function LOLCallingThree() { return YoLetsReturnThree() ; }; LOLCallingThree();})();").
{stdout,[<<"(function(){function a(){return 3}function b(){return a()}b()})();\n">>]}
5> stdinout:start_link(yui_js, "/usr/java/latest/bin/java -jar /home/matt/./repos/yuicompressor/build/yuicompressor-2.4.8pre.jar --type js").
{ok,<0.42.0>}
6> stdinout:send(yui_js, "(function() { function YoLetsReturnThree(){ return 3 ; } function LOLCallingThree() { return YoLetsReturnThree() ; }; LOLCallingThree();})();").
{stdout,[<<"(function(){function b(){return 3}function a(){return b()}a()})();">>]}
```

Note: `cl-uglify-js` returned the result in an average of 20ms.
`closure.jar` returned the result in an average of 800ms.
`yuicompressor.jar` returned the result in an average of 107ms.

### Erlang API STDIN->STDOUT->STDIN->...->STDOUT Pipes


### Network API Usage
Start a stdinout server with an explicit IP/Port to bind:

```erlang
93> stdinout:start_link(bob_network, "/home/matt/bin/cl-uglify-js", "127.0.0.1", 6641).
{ok,<0.10209.0>}
```

Now from a shell, send some data:

```bash
matt@vorash:~% echo "(function() { function YoLetsReturnThree(){ return 3 ; } function LOLCallingThree() { return YoLetsReturnThree() ; }; LOLCallingThree();})();" | nc localhost 6641
STDINOUT_POOL_ERROR: Length line too long: [(function(] (first ten bytes).
```

Uh oh, what went wrong?  We have to tell the server how big our input is going
to be first.  The protocol for the network server is simple: the first line
must contain the number of bytes of content you send.  The line is terminated
by a unix newline (i.e. "\n").

Trying again, we store the JS in a variable, use wc to get the length, then
send the length on the first line and the content after it:

```bash
matt@vorash:~% SEND_JS="(function() { function YoLetsReturnThree(){ return 3 ; } function LOLCallingThree() { return YoLetsReturnThree() ; }; LOLCallingThree();})();"
matt@vorash:~% echo $SEND_JS |wc -c
142
matt@vorash:~% echo $SEND_JS |(wc -c && echo $SEND_JS) | nc localhost 6641
(function(){function b(){return a()}function a(){return 3}b()})();
```

Success!  Here is the same example with wc/cat/nc goodness, but from a file:

```bash
matt@vorash:~% cat send_example.js
(function() { function YoLetsReturnThree(){ return 3 ; } function LOLCallingThree() { return YoLetsReturnThree() ; }; LOLCallingThree();})();
matt@vorash:~% wc -c send_example.js | (awk '{print $1}' && cat send_example.js )| nc localhost 6641
(function(){function b(){return a()}function a(){return 3}b()})();
```

Now you have a fully functioning stdin/stdout server accessible from erlang
or from the network.


Building
--------
Dependencies:

        rebar get-deps

Build:

        rebar compile


Testing
-------
Automated:

        rebar eunit skip_deps=true

Automated with timing details:

        rebar eunit skip_deps=true -v

In the `test/` directory there is a short script to verify error conditions.
You can load test error conditions with:

        time seq 0 300 |xargs -n 1 -P 16 ./errors.sh TARGET-IP TARGET-PORT

`P` is the number of time to run the script in parallel.  Increase or decrease
as necessary.

You can load test sending other data to stdinout over the network too, but
those tests can be target-specific depending on what you are spawning, your
memory usage of spawned processes, and overall workload expectations.

Tests work properly under Linux, but two tests fail under OS X due to spacing
differences in output.  You can visually spot check those to make sure they
are essentially the same.

History
-------
I wanted to get on-the-fly javascript minification done as fast as possible.
For small to medium-size JS snippets, starting up a Lisp or Java VM was
the dominating factor for throughput (too many `os:cmd/1` calls).

The obvious next step was to start up some processes, let them sit idle,
send stdin when needed, swallow output, then re-spawn dead processes.
It seemed so simple,
except erlang is unable to close stdin or send an EOF to stdin on spawned
processes.  Dammit.  The time came to dig out pipe/dup2 man pages and
make a stdin/stdout child process forwarder.  Thus, a stdin/stdout
proxy named stdin_forcer.c was born.

Tying everything back together, there is now a way to close stdin on spawned
programs.  But, what good is tying a general
concept like "data in, data out" to an erlang-only API?  Not very good.  Why
not glue on a network server so anybody can send stdin and get stdout?  There
is no good reason it should not exist, so now the erlang stdin/stdout forwarder
has a network API.  Potential usage: cluster of machines to on-the-fly minify
JS (use cl-uglify-js), CSS (yuicompressor.jar --type css), or transform
anything else you can shove data into and get something useful back out.
