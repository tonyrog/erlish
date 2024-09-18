%% wrapper for the receive function

-module(erlish_api).

-export([call/3]).
-export([signal/2]).
-export(['receive'/3]).  %% called from parse transform
-export([get_state/0]).
-export([put_state/1]).

-export([handle_signal/3]).  %% default signal handler

call(Pid, F, As) ->
    Ref = make_ref(),
    From = [self()|Ref],
    Pid ! {'$call', From, F, As},
    receive
	{Ref, Value} -> Value
    end.

signal(Pid, Signal) ->
    Ref = make_ref(),
    From = [self()|Ref],
    Pid ! {'$signal', From, Signal},
    receive
	{Ref, Value} -> Value
    end.

reply([Pid|Ref], Value) ->
    Pid ! {Ref, Value}.


'receive'(Mod, Fun, Timeout) ->
    case erlish_s:'receive'(Fun, Timeout) of
	{0, {'$call', From, F, As}} ->
	    ok = handle_call(Mod, From, F, As),
	    'receive'(Mod, Fun, Timeout);
	Head ->
	    Head
    end.

get_state() ->
    case get('$state') of
	S when is_map(S) -> S;
	_ -> #{}
    end.

put_state(S) ->
    put('$state', S).
		      
handle_call(Mod, From, F, As) ->
    S = get_state(),
    S1 = handle_call(Mod, From, F, As, S),
    put_state(S1),
    ok.

handle_call(Mod, From, F, As, S) ->
    io:format("got call ~w, ~w, ~w\n", [From, F, As]),
    case maps:get(F, Mod:api(), undefined) of
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
