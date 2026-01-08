-module(geometry).

-export([area/1]).

area({rectangle, Height, Width}) ->
    Height * Width;
area({square, Side}) ->
    Side * Side.
