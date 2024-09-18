-module(recv3).

-compile({parse_transform, erlish_t}).

-export([start/0]).
-export([test/0]).
-export([server_loop/1]).

-export([set/3]).
-export([get/2]).
-export([f/3]).
-export([g/3]).

-export([api/0]).

-export([set_/3]).
-export([get_/2]).
-export([f_/3]).
-export([g_/3]).

-define(reply(From, Value), hd(From) ! {tl(From),Value}).

start() ->
    spawn(fun() -> server_loop(#{}) end).

api() ->
	#{
	  set => fun set_/3,
	  get => fun get_/2, 
	  f => fun f_/3,
	  g => fun g_/3
	 }.

set(ServerRef, Key, Value) -> 
    erlish_api:call(ServerRef, set, [Key, Value]).
get(ServerRef, Key) ->
    erlish_api:call(ServerRef, get, [Key]).
f(ServerRef, A, B) ->
    erlish_api:call(ServerRef, f, [A, B]).
g(ServerRef, A, B) ->
    erlish_api:call(ServerRef, g, [A, B]).

server_loop(Opts) ->
    io:format("S = ~p\n", [erlish_api:get_state()]),
    receive
	next -> 
	    erlang:display(next),
	    ok
    end,
    receive
	{cudle, Pid} ->
	    erlang:display(cudle),
	    Pid ! gully,
	    server_loop(Opts)
    after 0 ->
	    server_loop(Opts)	    
    end.

%% local functions
set_(S, Key, Value) -> {reply, ok, S#{ Key=>Value }}.
get_(S, Key) -> {reply, maps:get(Key, S, undefined), S}.
f_(S, A, B) -> {reply, A + B, S}.
g_(S, A, B) -> {reply, A * B, S}.

test() ->
    Pid = start(),
    erlish_api:signal(Pid, {poke, bar, 200}),

    set(Pid, foo, 100),
    erlish_api:signal(Pid, {poke, bar, 300}),
    Pid ! next,

    io:format("get(foo) = ~p~n", [get(Pid, foo)]),
    Pid ! next,

    io:format("f(3, 4) = ~p~n", [f(Pid, 3, 4)]),
    Pid ! next,

    io:format("g(3, 4) = ~p~n", [g(Pid, 3, 4)]),
    io:format("peek(bar) = ~p\n", 
	      [erlish_api:signal(Pid, {peek, bar})]),
    Pid ! next,

    erlish_api:signal(Pid, {poke, hello, 123}),

    Value1 = erlish_api:signal(Pid, {process_info, registered_name}),
    io:format("process_info(~w, registered_name) = ~p~n", [Pid,Value1]),
    Value2 = erlish_api:signal(Pid, {register, xyz}),
    io:format("register(~w, xyz) = ~p~n", [Pid,Value2]),

    Pid ! {cudle, self()},
    io:format("get(foo) = ~p~n", [get(xyz, foo)]),
    Pid ! next,

    receive
	gully -> 
	    io:format("got gully\n")
    end,
    ok.
    

