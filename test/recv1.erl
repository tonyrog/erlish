%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%     Replace receive with handle_signal
%%% @end
%%% Created : 30 Sep 2022 by Tony Rogvall <tony@rogvall.se>

-module(recv1).

-compile({parse_transform, erlish_t}).
-export([example1a/1]).
-export([test1/0]).
-export([test1_/0]).

-record(state, 
	{
	 a,
	 b,
	 c
	}).

example1a(S) ->
    erlang:display({enter_example1a, S}),
    receive
	{x, A} when A > S#state.a -> example1a(S#state{a=A,b=S#state.b-1});
	{y, B} when B > S#state.b -> example1a(S#state{b=2*B})
    after 1000 -> S
    end.

%% test

test1() ->
    Pid = spawn(fun() -> example1a(#state{a=3,b=3,c=3}) end),
    Pid ! {y,3}, 
    erlish_api:signal(Pid, {put,foo,100}),
    Pid ! {x,4}, 
    ok.
