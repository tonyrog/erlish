%% wrapper for the receive function

-module(erlish_api).

-export([call/2, reply/2]).
-export([rpc/3, rpc/4]).
-export([signal/2]).
-export(['receive'/3]).  %% called from parse transform
-export([get_state/0]).
-export([put_state/1]).

-export([handle_signal/3]).  %% default signal handler
%% debug
-export([caller/0]).

-define(dbg(F,A), io:format("~p: " F "\n", [self() | (A)])).
-define(RPC_TIMEOUT, 3000).
-define(SIGNAL_TIMEOUT, 3000).

rpc(ServerRef, F, As) ->
    rpc(ServerRef, F, As, ?RPC_TIMEOUT).

rpc(ServerRef, F, As, Timeout) when is_pid(ServerRef); is_atom(ServerRef) ->
    Mod = caller(),  %% ?or transform? module of "server" calling (if any)
    MRef = erlang:monitor(process, ServerRef),
    ?dbg("RPC CALL ~p ~w to ~p from ~p, ref=~p", [F, As, ServerRef, Mod, MRef]),
    ServerRef ! {'$rpccall', {self(),MRef}, F, As},
    case 'receive'(Mod,
		   fun(Mesg) ->
			   case Mesg of
			       {'$rpccall', _From, _F, _As} -> {0, Mesg};
			       {MRef, _Value} -> {1, Mesg};
			       {'DOWN', MRef, _, _, _Reason} -> {2,Mesg};
			       _ -> nomatch
			   end
		   end, Timeout) of
	{1, {_, Value}} ->
            erlang:demonitor(MRef, [flush]),
	    Value;
	{2, {_, _, _, _, Reason}} ->
	    exit(Reason);
	timeout ->
	    {error, timeout}
    end.

signal(ServerRef, Request) when is_pid(ServerRef); is_atom(ServerRef) ->
    Mod = caller(),  %% ?or transform?
    MRef = erlang:monitor(process, ServerRef),
    ?dbg("SEND signal ~p to ~p from ~p, ref=~p", [Request,ServerRef,Mod,MRef]),
    ServerRef ! {'$signal',{self(),MRef},Request},
    case 'receive'(Mod,
		   fun(Mesg) ->
			   case Mesg of
			       {'$rpccall', _From, _F, _As} -> {0, Mesg};
			       {MRef, _Value} -> {1, Mesg};
			       {'DOWN', MRef, _, _, _Reason} -> {2,Mesg};
			       _ -> nomatch
			   end
		   end, ?SIGNAL_TIMEOUT) of
	{1, {_, Value}} -> 
            erlang:demonitor(MRef, [flush]),
	    Value;
	{2, {_, _, _, _, Reason}} ->
	    exit(Reason);
	timeout ->
	    {error, timeout}
    end.

call(ServerRef, Call) when is_atom(ServerRef); is_pid(ServerRef) ->
    MRef = erlang:monitor(process, ServerRef),
    ?dbg("CALL ~p to ~p, ref=~p", [Call,ServerRef,MRef]),
    ServerRef ! {call,{self(),MRef}, Call},
    receive
	{MRef, Reply} -> 
            erlang:demonitor(MRef, [flush]),
	    Reply;
	{'DOWN', MRef, _, _, Reason} ->
	    exit(Reason)
    end.

reply({Pid,Ref}, Value) ->
    Pid ! {Ref, Value}.

'receive'(Mod, Fun, Timeout) ->
    ?dbg("RECEIVE(~p ~p ~p)", [Mod, Fun, Timeout]),
    case prim_eval:'receive'(Fun, Timeout) of
	{0, {'$rpccall', From, F, As}} ->
	    ok = handle_rpc(Mod, From, F, As),
	    %% fixme! update timeout if != infinity
	    'receive'(Mod, Fun, Timeout);
	{-1, {'$signal', From, Request}} ->
	    try handle_signal(Mod, From, Request) of
		_Result ->
		    %% fixme! update timeout if != infinity
		    'receive'(Mod, Fun, Timeout)
	    catch
		error:Reason:Stack ->
		    reply(From, {error, Reason}), %% possible double reply?
		    io:format("signal crash: ~p~n", [Reason]),
		    io:format("Stack:\n~p\n", [Stack]),
		    'receive'(Mod, Fun, Timeout)
	    end;
	Head ->
	    ?dbg("RECEIVE MATCHED ~p", [Head]),
	    Head
    end.


%% caller module to erlish_api 
caller() ->
    try caller_stk()
    catch
	throw:stk:Stack ->
	    %% io:format("Stack: ~p~n", [Stack]),
	    case Stack of
		[_,_,_,{M,_F,_Arity,_}|_] -> M;
		_ -> undefined
	    end
    end.

caller_stk() ->
    throw(stk).

get_state() ->
    case get('$state') of
	S when is_map(S) -> S;
	_ -> #{}
    end.

put_state(S) ->
    put('$state', S).

get_api(undefined) -> #{};
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
    ?dbg("HANDLE_RPC(mod=~w,from=~w,f=~w,as=~w)", [Mod,From,F,As]),
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
    ?dbg("HANDLE_SIGNAL(mod=~p,from=~w,rquest=~p", [_Mod,From,Request]),
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
	{unregister, Name} ->
	    reply(From, erlang:unregister(Name));
	_ ->
	    reply(From, {error, unknown}),
	    ok
    end.
