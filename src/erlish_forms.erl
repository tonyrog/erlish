%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%	 Parse file and emit forms
%%% @end
%%% Created : 12 Sep 2024 by Tony Rogvall <tony@rogvall.se>

-module(erlish_forms).

-compile(export_all).

file(File) ->
    {ok, Fs} = epp:parse_file(File, [], []),
    {ok, Fd} = file:open(File++".forms", [write]),
    lists:foreach(fun(P) -> io:format(Fd, "~p.\n\n", [P]) end, Fs).
