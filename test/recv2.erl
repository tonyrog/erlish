-module(recv2).

-compile({parse_transform, erlish_t}).

-export([example1b/1]).
-export([test/0]).

-record(state, 
	{
	 a,
	 b,
	 c
	}).

example1b(S) ->
    io:format("example1b: ~p\n", [S]),
    receive
	{x, A} when A > S#state.a ->
	    example1b(S#state{a=A,b=S#state.b-1});
	{y, B} when B > S#state.b ->
	    example1b(S#state{b=2*B})
    after 1000 -> 
	    S
    end.

test() ->
    Pid = spawn(fun() -> example1b(#state{a=3,b=3,c=3}) end),
    Pid ! {y,3}, 
    erlish_api:signal(Pid, {poke,foo,100}),
    Pid  ! {x,4},
    ok.
