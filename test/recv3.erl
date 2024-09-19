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

-rpc([set_/3, get_/2, r_/3, g_/3]).

start() ->
    spawn(fun() -> server_loop(#{}) end).

%% client API
set(ServerRef, Key, Value) -> 
    erlish_api:rpc(ServerRef, set, [Key, Value]).
get(ServerRef, Key) ->
    erlish_api:rpc(ServerRef, get, [Key]).
f(ServerRef, A, B) ->
    erlish_api:rpc(ServerRef, f, [A, B]).
g(ServerRef, A, B) ->
    erlish_api:rpc(ServerRef, g, [A, B]).

%% RPC API functions
api() ->
	#{
	  set => fun set_/3,
	  get => fun get_/2, 
	  f => fun f_/3,
	  g => fun g_/3
	 }.

%% local functions
set_(S, Key, Value) -> {reply, ok, S#{ Key=>Value }}.
get_(S, Key) -> {reply, maps:get(Key, S, undefined), S}.
f_(S, A, B) -> {reply, A + B, S}.
g_(S, A, B) -> {reply, A * B, S}.


server_loop(Opts) ->
    io:format("SERVER_LOOP: S = ~p\n", [erlish_api:get_state()]),
    receive
	next -> 
	    io:format("  NEXT\n"),
	    ok
    end,
    receive
	{cudle, Pid} ->
	    io:format("  CUDLE\n"),
	    Pid ! gully,
	    server_loop(Opts)
    after 0 ->
	    server_loop(Opts)	    
    end.


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

    case erlish_api:signal(Pid, {process_info, registered_name}) of
	[] ->
	    true = erlish_api:signal(Pid, {register, xyz});
	xyz ->
	    true
    end,

    Pid ! {cudle, self()},
    io:format("get(xyz, foo) = ~p~n", [get(xyz, foo)]),
    Pid ! next,
	    
    Value2 = erlish_api:signal(Pid, {unregister, xyz}),
    io:format("unregister(xyz) = ~p~n", [Value2]),

    receive
	gully -> 
	    io:format("got gully\n")
    after 1000 ->
	    io:format("no gully\n")
    end,
    ok.
    

