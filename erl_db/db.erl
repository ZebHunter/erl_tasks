-module(db).
-export([new/0,
    destroy/1,
    write/3,
    delete/2,
    read/2,
    match/2]).

new() -> spawn(fun() -> server(#{}) end).

server(Map) when is_map(Map) -> 
    receive
        {write, Key, Element} -> server(Map#{Key => Element});
        {delete, Key} -> server(maps:remove(Key, Map));
        {read, Key, From} -> 
            case maps:find(Key, Map) of
                {ok, Element} -> From ! {ok, Element};
                error -> From ! {error, not_found}
            end,
            server(Map);
        {match, Element, From} -> 
            From ! [Key || {Key, E} <- maps:to_list(Map), E =:= Element],
            server(Map);
        stop -> ok;
        _ -> {error, unknown}
    end.

destroy(Pid) when is_pid(Pid) -> Pid ! stop,
    ok.

write(Key, Element, Pid) -> 
    Pid ! {write, Key, Element},   
    ok.

delete(Key, Pid) -> 
        try Pid ! {delete, Key}
        catch _:Reason -> {error, Reason}
    end,
    ok.

read(Key, Pid) -> 
    Pid ! {read, Key, self()},
    receive
        {ok, Element} -> {ok, Element};
        {error, Reason} -> {error, Reason}
    end.

match(Element, Pid) -> 
    try Pid ! {match, Element, self()},
        receive
            Keys -> Keys
        end
    catch _:Reason -> {error, Reason}
    end.

