%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%     Demonstrate two servers calling each other
%%% @end
%%% Created : 15 Sep 2024 by Tony Rogvall <tony@rogvall.se>

-module(recv5).

-compile({parse_transform, erlish_t}).

-export([start/0]).
-export([test/0]).
-export([server_loop/0]).
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
    spawn(fun() -> server_loop() end).

server_loop() ->
    receive
	stop -> ok
    after infinity -> ok
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
		  erlish_api:signal(Pid1, {process_info, message_queue_len})
	  end),
    Value = foo(Pid1, Pid2),
    Pid1 ! stop,
    Pid1 ! stop,
    Value.
