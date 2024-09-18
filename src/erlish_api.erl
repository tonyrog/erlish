%% wrapper for the receive function

-module(erlish_api).

-export([call/2, reply/2]).
-export([rpc/3]).
-export([signal/2]).
-export(['receive'/3]).  %% called from parse transform
-export([get_state/0]).
-export([put_state/1]).

-export([handle_signal/3]).  %% default signal handler

-define(RPC_TIMEOUT, 3000).
-define(SIGNAL_TIMEOUT, 1000).

rpc(ServerRef, F, As) when is_pid(ServerRef); is_atom(ServerRef) ->
    Mod = caller(),  %% ?or transform? module of "server" calling (if any)
    io:format("rpc from ~p\n", [Mod]),
    Ref = make_ref(),  %% monitor!
    From = [self()|Ref],
    ServerRef ! {'$rpccall', From, F, As},
    case 'receive'(Mod,
		   fun(Mesg) ->
			   case Mesg of
			       {'$rpccall', _From, _F, _As} -> {0, Mesg};
			       {Ref, _Value} -> {1, Mesg};
			       _ -> nomatch
			   end
		   end, ?RPC_TIMEOUT) of
	{1, {_, Value}} ->
	    Value;
	timeout ->
	    {error, timeout}
    end.

signal(ServerRef, Signal) when is_pid(ServerRef); is_atom(ServerRef) ->
    Mod = caller(),  %% ?or transform?
    io:format("signal from ~p\n", [Mod]),
    Ref = make_ref(),  %% monitor!
    From = [self()|Ref],
    ServerRef ! {'$signal', From, Signal},
    case 'receive'(Mod,
		   fun(Mesg) ->
			   case Mesg of
			       {'$rpccall', _From, _F, _As} -> {0, Mesg};
			       {Ref, _Value} -> {1, Mesg};
			       _ -> nomatch
			   end
		   end, ?SIGNAL_TIMEOUT) of
	{1, {_, Value}} -> 
	    Value;
	timeout ->
	    {error, timeout}
    end.

call(ServerRef, Call) when is_atom(ServerRef); is_pid(ServerRef) ->
    Ref = make_ref(),
    From = [self()|Ref],
    ServerRef ! {call, From, Call},
    receive
	{Ref, Value} -> Value
    end.

reply([Pid|Ref], Value) ->
    Pid ! {Ref, Value}.

'receive'(Mod, Fun, Timeout) ->
    process_signals(Mod),
    case erlish_s:'receive'(Fun, Timeout) of
	{0, {'$rpccall', From, F, As}} ->
	    ok = handle_rpc(Mod, From, F, As),
	    'receive'(Mod, Fun, Timeout);
	Head ->
	    Head
    end.

process_signals(Mod) ->
    receive
	{'$signal', From, Request} ->
	    handle_signal(Mod, From, Request),
	    process_signals(Mod)
    after 0 ->
	    ok
    end.

%% caller module to erlish_api 
caller() ->
    try throw(stk)
    catch
	throw:stk:Stack ->
	    io:format("Stack: ~p~n", [Stack]),
	    [_,_,{M,_F,_Arity,_}|_] = Stack,
	    M
    end.


get_state() ->
    case get('$state') of
	S when is_map(S) -> S;
	_ -> #{}
    end.

put_state(S) ->
    put('$state', S).

get_api(Mod) ->
    try Mod:api() of
	Api -> Api
    catch
	error:undef -> #{}
    end.
		      
handle_rpc(Mod, From, F, As) ->
    S = get_state(),
    S1 = handle_rpc(Mod, From, F, As, S),
    put_state(S1),
    ok.

handle_rpc(Mod, From, F, As, S) ->
    io:format("got rpc ~w, ~w, ~w\n", [From, F, As]),
    case maps:get(F, get_api(Mod), undefined) of
	undefined -> 
	    io:format("unknown function ~w~n", [F]),
	    reply(From, {error, undef}),
	    S;
	Fun ->
	    {arity,Arity} = erlang:fun_info(Fun, arity),
	    case 1+length(As) =:= Arity of
		true ->
		    try apply(Fun, [S|As]) of
			{reply, Value, S1} ->
			    reply(From, Value),
			    S1;
			{noreply, S1} ->
			    S1
		    catch
			error:Error ->
			    reply(From, {error, Error}),
			    S
		    end;
		false ->
		    io:format("arity mismatch ~w ~w~n", [F, As]),
		    reply(From, {error, badarity}),
		    S
	    end
    end.

%% default handle
%% handle signals that affect process 
%% link/monitor/flags (no receive is allowed)
handle_signal(_Mod, From, Request) ->
    %% fixme check if Mod has an override!
    erlang:display({handle_signal,Request}),
    case Request of
	{process_info, Item} ->
	    reply(From, erlang:process_info(self(), Item));
	{process_flag, Flag, Value} ->
	    reply(From, erlang:process_flag(Flag, Value));
	{trap_exit, Bool} ->
	    reply(From, erlang:process_flag(trap_exit, Bool));
	{poke, Key, Value} ->
	    reply(From, erlang:put(Key, Value));
	{peek, Key} ->
	    reply(From, erlang:get(Key));
	{register, Name} ->
	    reply(From, erlang:register(Name, self()));
	_ -> 
	    ok
    end.
