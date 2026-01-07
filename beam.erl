-module(beam).

-export([start/0]).

start() ->
    case gen_tcp:listen(6969, [binary, {packet, 0}, {reuseaddr, true}]) of
        {ok, LSock} ->
            do_something(LSock);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

do_something(LSock) ->
    io:format("Listening on ~p~n", [LSock]),
    ok.
