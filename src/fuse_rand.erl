-module(fuse_rand).

-export([uniform/0]).

uniform() ->
    rand:uniform().
