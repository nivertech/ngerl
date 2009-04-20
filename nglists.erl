%%% @doc Additional list functions.
%%% @author Hasan Veldstra <hasan@12monkeys.co.uk>
%%% license: BSD

-module(nglists).
-export([count/2, deepmap/2, drop/2, init/1, pos/2, uniq/1]).

-import(lists, [all/2, append/1, duplicate/2, flatten/1, foldl/3, foreach/2,
                map/2, nth/2, nthtail/2, reverse/1, seq/2, sum/1, zip/2,
                zipwith/3]).


%%% @doc  Count the number of list items that satisfy some predicate.

count(Fun, L) ->
    length(lists:filter(Fun, L)).


%% @doc Like map, but maintains structure of deep lists.

deepmap(Fun, [H|T]) when list(H) -> [deepmap(Fun, H) | deepmap(Fun, T)];
deepmap(Fun, [H|T])              -> [Fun(H) | deepmap(Fun,T)];
deepmap(_Fun, [])                -> [].


%% @doc Drop N elements from list L.

drop(N, L) when N > 0 -> nthtail(N, L);
drop(N, L) when N < 0 -> reverse(nthtail(N * -1, reverse(L)));
drop(_, L)            -> L.


%% @doc Drop the last element of a list.

init(L) -> reverse(tl(reverse(L))).


%% @doc Find the position of element E in list L.

pos(E, L)                    -> pos(E, L, 1).
pos(_, [], _)                -> 0;
pos(E, [H|_], I) when E == H -> I;
pos(E, [_|T], I)             -> pos(E, T, I+1).


%% @doc Remove duplicates from list L.

uniq([H|T]) -> [H | uniq([Y || Y <- T, Y =/= H])];
uniq([])    -> [].
