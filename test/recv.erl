%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Hardcore erlang gen_server 
%%% @end
%%% Created :  7 Oct 2020 by Tony Rogvall <tony@rogvall.se>

-module(recv).
%% -compile({parse_transform, recv_t}).

-export([seq/0, nest/0]).
-export([seq/1, nest/1]).
-export([call/2]).

%% sys
-export([system_continue/3]).
-export([system_terminate/4]).
%% -export([system_get_state/1]).
%% -export([system_replace/2]).
%% -export([format_status/2]).
%% -export([system_code_change/4]).
%% -export([format_status/2]).

-define(SYS(State,Loop),
	{system,From,Request} ->
	       Loop(sys:handle_system_msg(Request, From, get_parent(), ?MODULE,
					  get_debug_options(), State))).

seq() ->
    spawn_serv(?MODULE, seq, [0]).

nest() ->
    spawn_serv(?MODULE, nest, [0]).

spawn_serv(Mod, Fun, Args) ->
    Parent = self(),
    spawn(fun() -> enter(Parent, Mod, Fun, Args) end).

%% sequential selective receive
seq(S0) ->
    io:format("seq ~w\n", [S0]),
    S1 = fun Loop1(Si) ->
		 io:format("seq loop1 ~w\n", [Si]),
		 receive
		     foo -> io:format("foo\n"), Si+1;
		     bar -> io:format("bar\n"), Si+1;
		     ?SYS(Si,Loop1)
		  end
	 end(S0),
    S2 = fun Loop2(Si) ->
		 io:format("seq loop2 ~w\n", [Si]),
		 receive
		     kalle ->  io:format("kalle\n"), Si+1;
		     pelle ->  io:format("pelle\n"), Si+1;
		     ?SYS(Si,Loop2)
		 end
	 end(S1),
    seq(S2).

%% nested selective receive
nest(S0) ->
    io:format("nest ~w\n",[S0]),
    S1 = fun Loop1(Si) ->
		 io:format("nest loop1 ~w\n", [Si]),
		 receive
		     foo ->
			 S1 = begin io:format("foo\n"), Si+1 end,
			 fun Loop2(Sj) ->
				 io:format("nest foo loop2 ~w\n", [Sj]),
				 receive
				     {call,From,a} -> 
					 io:format("call a\n"), 
					 response(From, aa),
					 Sj+1;
				     {call,From,b} -> 
					 io:format("call b\n"), 
					 response(From, bb),
					 Sj+1;
				     c ->  
					 io:format("message c\n"), 
					 Sj+1;
				     ?SYS(S1,Loop2)
				     end
			 end(S1);
		     bar -> 
			 S1 = begin io:format("bar\n"), S0+1 end,
			 fun Loop2(Sj) ->
				 io:format("nest bar loop2 ~w\n", [Sj]),
				 receive
				     {call,From,a} -> 
					 io:format("call a\n"), 
					 response(From, aaa),
					 Sj+1;
				     {call,From,b} -> 
					 io:format("call b\n"), 
					 response(From, bbb),
					 Sj+1;
				     a ->  io:format("a\n"), Sj+1;
				     b ->  io:format("b\n"), Sj+1;
				     ?SYS(S1,Loop2)
				     end
			 end(S1);
		     ?SYS(S0,Loop1)
		     end
	 end(S0),
    nest(S1).


enter(Parent, Mod, Fun, Args) ->
    put(parent, Parent),
    put(debug_options, []),
    apply(Mod, Fun, Args).

call(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {call,{self(),Ref},Request},
    receive
	{Ref,Response} -> Response
    end.

response({Pid,Ref}, Response) ->
    Pid ! {Ref, Response}.

get_parent() ->
    get(parent).

get_debug_options() ->
    case get(debug_options) of
	undefined -> [];
	Options -> Options
    end.

system_continue(_Parent, _Debug, Misc) ->
    io:format("continue\n"),
    Misc.

system_terminate(Reason, _Parent, _Debug, _Misc) ->
    io:format("terminate: ~p\n", [Reason]),
    exit(Reason).

