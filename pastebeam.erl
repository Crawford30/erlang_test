-module(pastebeam).
-export([start/0]).

start() -> 
    {ok, LSock} = gen_tcp:listen(6699, [binary, {packet, 0}, {reuseaddr, true}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    gen_tcp:send(Sock, "Hello"),
    gen_tcp:close(Sock) .