%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%     Replace receive with handle_signal
%%% @end
%%% Created : 30 Sep 2022 by Tony Rogvall <tony@rogvall.se>

-module(recv1).

-compile({parse_transform, erlish_t}).
-export([example1a/1]).
-export([test/0]).

-record(state, 
	{
	 a,
	 b,
	 c
	}).

example1a(S) ->
    io:format("example1a: ~p\n", [S]),
    receive
	{x, A} when A > S#state.a -> 
	    example1a(S#state{a=A,b=S#state.b-1});
	{y, B} when B > S#state.b -> 
	    example1a(S#state{b=2*B})
    after 1000 ->
	    S
    end.

%% test

test() ->
    Pid = spawn(fun() -> example1a(#state{a=3,b=3,c=3}) end),
    Pid ! {x,4}, 
    erlish_api:signal(Pid, {poke,foo,100}),
    Pid ! {y,3},
    ok.
