-file("recv2_p.erl", 1).

-module(recv2_p).

-export([example1b/1]).

-export([test/0]).

-record(state,{a, b, c}).

example1b(S) ->
    erlang:display({enter_example1b, S}),
    case
        erlish_api:'receive'(recv2_p,
                             fun(Mesg__1) ->
				     erlang:display({recv2_p,S,Mesg__1}),
                                    case Mesg__1 of
                                        {'$call', _From__2, _Func__3,
                                         _Args__4} ->
                                            {0, Mesg__1};
                                        {'$signal', From__5, Request__6} ->
                                            erlish_api:handle_signal(recv2_p,
                                                                     From__5,
                                                                     Request__6),
                                            match;
                                        {x, A} when A > S#state.a ->
                                            {1, Mesg__1};
                                        {y, B} when B > S#state.b ->
                                            {2, Mesg__1};
                                        _ ->
                                            nomatch
                                    end
                             end,
                             1000)
    of
        {1, {x, A}} ->
	    erlang:display({match,1}),
            example1b(S#state{a = A, b = S#state.b - 1});
        {2, {y, B}} ->
	    erlang:display({match,2}),
            example1b(S#state{b = 2 * B});
        timeout ->
            S
    end.

test() ->
    Pid =
        spawn(fun() ->
                     example1b(#state{a = 3, b = 3, c = 3})
              end),
    Pid ! {y, 3},
    erlish_api:signal(Pid, {poke, foo, 100}),
    Pid ! {x, 4},
    ok.



