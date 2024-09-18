%% Tiny Erlish example
-module(tiny).

-compile({parse_transform, erlish_t}).

-export([api/0]).
-export([start/0]).
-export([init/1]).
-export([add/2]).
-export([print/1]).
-export([stop/1]).

-export([test/0]).

api() ->
    #{
      init =>
	  fun (S) -> {reply, ok, S#{ x => 0 }} end,
      add => 
	  fun (S=#{ x := X}, Value) -> {reply, ok, S#{ x => X+Value }} end,
      print =>
	  fun (S=#{ x:= X}) -> io:format("x=~p~n", [X]), {reply, ok, S} end
     }.

start() ->
    spawn(fun() -> loop() end).

loop() ->
    receive
	stop -> ok
    end.

init(Pid) ->
    erlish_api:call(Pid, init, []).
add(Pid,Value) ->
    erlish_api:call(Pid, add, [Value]).
print(Pid) ->
    erlish_api:call(Pid, print, []).
stop(Pid) ->
    Pid ! stop.

test() ->
    Pid = start(),
    init(Pid),
    add(Pid, 1),
    add(Pid, 2),
    add(Pid, 3),
    add(Pid, 4),
    add(Pid, 5),
    print(Pid),
    stop(Pid).
