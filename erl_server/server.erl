-module(server).
-export([connect/0, 
    calculate/2]).

connect() -> spawn(fun() -> loop() end).

loop() -> 
    receive
        {From, Request} when is_pid(From) ->
            Result = calculate(Request),
            From ! Result
        end,
    loop().

calculate(Pid, Oper) -> 
    From = self(),
    Pid ! {From, Oper},
    receive
        Reply -> Reply
    end.

calculate(Oper) ->
    case Oper of
        {add, Args} when is_list(Args) -> calculate_sum(Args);
        {mul, Args} when is_list(Args) -> calculate_mul(Args);
        {sub, Args} when is_list(Args) -> calculate_sub(Args);
        {divide, Args} when is_list(Args) -> calculate_div(Args);
        X when is_number(X) -> X
    end.

calculate_sum(Args) -> 
    lists:foldl(fun(Arg, Acc) -> calculate(Arg) + Acc end, 0, Args).

calculate_mul(Args) ->
    lists:foldl(fun(Arg, Acc) -> calculate(Arg) * Acc end, 1, Args).

calculate_sub(Args) -> 
    lists:foldl(fun(Arg, Acc) -> Acc - calculate(Arg) end, hd(Args), tl(Args)).

calculate_div(Args) ->
    lists:foldl(fun(Arg, Acc) -> Acc / calculate(Arg) end, hd(Args), tl(Args)).

