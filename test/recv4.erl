%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%     Demonstrate two servers calling each other
%%% @end
%%% Created : 15 Sep 2024 by Tony Rogvall <tony@rogvall.se>

-module(recv4).

-compile({parse_transform, erlish_t}).

-export([start/0]).
-export([test/0]).
-export([server_loop/1]).
-export([api/0]).
-export([foo/2]).
-export([bar/2]).

api() ->
    #{ foo => fun foo_/2, 
       bar => fun bar_/2
     }.

foo(Pid, A) ->
    erlish_api:rpc(Pid, foo, [A]).

bar(Pid, B) -> 
    erlish_api:rpc(Pid, bar, [B]).

start() ->
    spawn(fun() -> server_loop(#{}) end).

server_loop(S) ->
    io:format("~w: S = ~p\n", [self(), S]),
    receive
	stop -> 
	    ok
    end.


foo_(S, Arg) when is_pid(Arg) -> 
    X = bar(Arg, 11),
    {reply, X+1, S#{ me => foo }}.
    
bar_(S, Arg) -> 
    timer:sleep(2000),
    {reply, Arg + 3, S#{ me => bar} }.

test() ->
    Pid1 = start(),
    Pid2 = start(),
    spawn(fun() -> 
		  timer:sleep(1000),
		  L1 = erlish_api:signal(Pid1,{process_info,message_queue_len}),
		  io:format("Pid1 message queue length ~p\n", [L1]),
		  L2= erlish_api:signal(Pid2,{process_info,message_queue_len}),
		  io:format("Pid2 message queue length ~p\n", [L2]),
		  ok
	  end),
    Value = foo(Pid1, Pid2),
    Pid1 ! a, Pid1 ! b, Pid1 ! c,
    Pid2 ! a, Pid2 ! b, Pid2 ! c,
    timer:sleep(1500),
    Pid1 ! stop,
    Pid2 ! stop,
    Value.
