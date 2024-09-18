%% parse transform for recv
-module(erlish_t).

-export([format_error/1]).
-export([parse_transform/2]).

%% Current dictionary use:
%%  '$var' next variable number
%%
new_var(BaseName) ->
    N = case get('$var') of
	     undefined -> 1;
	     N0 -> N0
	 end,
    put('$var', N+1),
    list_to_atom(BaseName ++ integer_to_list(N)).

reset_var() ->
    put('$var', 1).
    

parse_transform(Forms, _Options) ->
    reset_var(),
    transform_forms(Forms, #{}).

transform_forms([F={attribute,_Ln,module,Mod}|Fs], S) ->
    [F | transform_forms(Fs, S#{ module => Mod })];
transform_forms([{function,Ln,Name,Arity,Body}|Fs], S) ->
    io:format("transforming function ~p/~p\n", [Name,Arity]),
    TBody = transform_body(Body,S),
    [{function,Ln,Name,Arity,TBody} | transform_forms(Fs, S)];
transform_forms([F|Fs], S) ->
	[F | transform_forms(Fs, S)];
transform_forms([], _S) ->
	[].

transform_body(Body, S) ->
    transform(
      fun(expr,{'receive',Ln, Clauses}) -> 
	      io:format("transforming receive\n"),
	      transform_recv(Ln, Clauses, {atom,Ln,infinity}, [], S);
	 (expr,{'receive',Ln, Clauses, Timeout, Action}) ->
	      io:format("transforming receive\n"),
	      transform_recv(Ln, Clauses, Timeout, Action, S);
	 (_W,F) -> 
	      F
      end, Body).

transform_recv(Ln, Clauses, Timeout, Action, S) ->
    Var = new_var("Mesg__"),
    {'case', Ln,
     {call, Ln,
      {remote,Ln,{atom,Ln,erlish_api},{atom,Ln,'receive'}},
      [{atom,Ln,maps:get(module, S)},
	{'fun',Ln, 
	{clauses,
	 [{clause,Ln,[{var,Ln,Var}],[],
	   [{'case',Ln,{var,Ln,Var},
	     [handle_call_clause(Var,Ln),
	      handle_signal_clause(Ln,S)] ++  %% override ?
		 transform_recv_clauses(Var, 1, Clauses) ++
		 [{clause,Ln,[{var,Ln,'_'}],[],[{atom,Ln,nomatch}]}]}]}
	  ]}},
       Timeout]},
     transform_case_clauses(Var, 1, Clauses) ++
	 if Action =:= [] -> [];
	    true ->[{clause,Ln,[{atom,Ln,timeout}],[],Action}]
	 end
    }.

handle_call_clause(Var,Ln) ->
    From = new_var("_From__"),
    Func  = new_var("_Func__"),
    Args  = new_var("_Args__"),
    {clause,Ln,
     [{tuple,Ln,
       [{atom,Ln,'$call'},{var,Ln,From},
	{var,Ln,Func},{var,Ln,Args}]}],
     [],
     [{tuple,Ln,[{integer,Ln,0},{var,Ln,Var}]}]}.

handle_signal_clause(Ln,S) ->
    From = new_var("From__"),
    Request  = new_var("Request__"),
    Mod = maps:get(module, S),
    {clause,Ln,
     [{tuple,Ln,
       [{atom,Ln,'$signal'},{var,Ln,From},{var,Ln,Request}]}],
     [],
     [{call,Ln,
       {remote,Ln,{atom,Ln,erlish_api},{atom,Ln,handle_signal}},
       [{atom,Ln,Mod},{var,Ln,From},{var,Ln,Request}]},
      {atom,Ln,match}]}.


transform_recv_clauses(Var, No, [{clause,Ln,Head,Guard,_Body}|Clauses]) ->
    [{clause,Ln,Head,Guard,
      [{tuple,Ln,[{integer,Ln,No},{var,Ln,Var}]}]} |
     transform_recv_clauses(Var, No+1, Clauses)];
transform_recv_clauses(_Var, _No, []) -> 
    [].

transform_case_clauses(Var, No, [{clause,Ln,[Head],_Guard,Body}|Clauses]) ->
    [{clause,Ln,
      [{tuple,Ln,[{integer,Ln,No},Head]}],[],Body} |
     transform_case_clauses(Var, No+1, Clauses)];
transform_case_clauses(_Var, _No, []) -> 
    [].


transform(Fun,Form) ->
    tf_(Fun,form,Form).

tf_(Fun,W,Form) ->
    Form1 =
	case Form of
	    {T,Ln} -> {T,Ln};
	    {'fun',_Ln,{function,_F,_A}} -> Form;
	    {'fun',_Ln,{function,_M,_F,_A}} -> Form;
	    {'fun',Ln,{clauses,Cs}} -> 
		{'fun',Ln,{clauses,tf_(Fun,clauses,Cs)}};
	    {'named_fun',Ln,Name,Cs} ->
		{'named_fun',Ln,Name,tf_(Fun,clauses,Cs)};
	    {T,Ln,A1} -> {T,Ln,tf_(Fun,W,A1)};
	    {T,Ln,A1,A2} -> {T,Ln,tf_(Fun,W,A1),tf_(Fun,W,A2)};
	    {clause,Ln,A1,A2,A3} ->
		{clause,Ln,tf_(Fun,head,A1),tf_(Fun,guard,A2),tf_(Fun,expr,A3)};
	    {T,Ln,A1,A2,A3} ->
		{T,Ln,tf_(Fun,W,A1),tf_(Fun,W,A2),tf_(Fun,W,A3)};
	    {'try',Ln,A1,A2,A3,A4} ->
		{'try',Ln,tf_(Fun,expr,A1),tf_(Fun,clauses,A2),
		 tf_(Fun,clauses,A3),tf_(Fun,expr,A4)};
	    [H|T] -> [tf_(Fun,W,H) | tf_(Fun,W,T)];
	    [] -> [];
	    _ when is_number(Form) -> Form;
	    _ when is_atom(Form) -> Form
	end,
    Fun(W,Form1).

format_error(Mesg) ->
    Mesg.
