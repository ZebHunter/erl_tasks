-module(server_test).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
    server:start_link(),
    Pid = server:connect(),
    io:format("~p: sends ~p~n", [self(), Pid]),
    ?assertEqual(6, server:calculate(Pid, {add, [1, 2, 3]})),
    Pid ! exit.

mul_test() ->
    server:start_link(),
    Pid = server:connect(),
    io:format("~p: sends ~p~n", [self(), Pid]),
    ?assertEqual(6, server:calculate(Pid, {mul, [1, 2, 3]})),
    Pid ! exit.