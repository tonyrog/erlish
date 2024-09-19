%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    On/Off state machine
%%% @end
%%% Created : 19 Sep 2024 by Tony Rogvall <tony@rogvall.se>

-module(moment).

-compile({parse_transform, erlish_t}).
-export([start/0, press/1, release/1]).
-export([test/0]).

start() ->
    spawn(fun() -> init() end).

press(Machine) ->
    Machine ! {button,press}.

release(Machine) ->
    Machine ! {button,release}.

init() ->
    off().

off() ->
    put('$this', off),
    io:format("Moment: OFF\n"),
    receive
	{button,press} ->
	    on();
	{button,release} ->
	    off();
	stop ->
	    ok
    end.

on() ->
    put('$this', on),
    io:format("Moment: ON\n"),
    receive
	{button,press} ->
	    on();
	{button,release} ->
	    off();
	stop ->
	    ok
    end.

test() ->
    Pid = start(),
    press(Pid), release(Pid),
    timer:sleep(1000),
    press(Pid), release(Pid),
    timer:sleep(1000),
    press(Pid), release(Pid),
    timer:sleep(1000),
    Value = erlish_api:signal(Pid, {peek,'$this'}),
    io:format("machine value = ~p\n", [Value]),
    Pid ! stop.

