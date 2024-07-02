-module(server).
-behaviour(gen_server).

-export([start_link/0, connect/0, calculate/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    gen_server:call(?MODULE, connect).

calculate(Pid, Oper) ->
    gen_server:call(Pid, {calculate, Oper}).

init([]) ->
    {ok, #state{}}.

handle_call(connect, _From, State) ->
    {reply, self(), State};

handle_call({calculate, Oper}, _From, State) ->
    try
        Result = calculate(Oper),
        {reply, Result, State}
    catch
        _:Reason ->
            {reply, {error, Reason}, State}
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

calculate(Oper) ->
    Fun = fun
        ({add, Args}) when is_list(Args) -> calculate_sum(Args);
        ({mul, Args}) when is_list(Args) -> calculate_mul(Args);
        ({sub, Args}) when is_list(Args) -> calculate_sub(Args);
        ({divide, Args}) when is_list(Args) -> calculate_div(Args);
        (X) when is_number(X) -> X
    end,
    Fun(Oper).

calculate_sum(Args) -> 
    lists:foldl(fun(Arg, Acc) -> calculate(Arg) + Acc end, 0, Args).

calculate_mul(Args) ->
    lists:foldl(fun(Arg, Acc) -> calculate(Arg) * Acc end, 1, Args).

calculate_sub(Args) -> 
    lists:foldl(fun(Arg, Acc) -> Acc - calculate(Arg) end, hd(Args), tl(Args)).

calculate_div(Args) ->
    try lists:foldl(fun(Arg, Acc) -> Acc / calculate(Arg) end, hd(Args), tl(Args))
    catch _:_ -> {error, divide_by_zero}
    end.