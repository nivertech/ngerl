%%% @doc Additional list functions.
%%% @author Hasan Veldstra <hasan@12monkeys.co.uk>
%%% license: BSD

-module(nglists).
-export([count/2, deepmap/2, drop/2,
         find_first/2, find_first/3, find_last/2, find_last/3,
         init/1, pos/2, transpose/1, uniq/1, shuffle/1, random/1]).

-import(lists, [all/2, append/1, duplicate/2, flatten/1, foldl/3, foreach/2,
                keysort/2, map/2, nth/2, nthtail/2, reverse/1, seq/2, sum/1, 
                zip/2, zipwith/3]).


%%% @doc  Count the number of list items that satisfy some predicate.

count(Fun, L) ->
    length(lists:filter(Fun, L)).


%% @doc Like map, but maintains structure of deep lists.

deepmap(Fun, [H|T]) when is_list(H) -> [deepmap(Fun, H) | deepmap(Fun, T)];
deepmap(Fun, [H|T])                 -> [Fun(H) | deepmap(Fun,T)];
deepmap(_Fun, [])                   -> [].


%% @doc Drop N elements from list L.

drop(N, L) when N > 0 -> nthtail(N, L);
drop(N, L) when N < 0 -> reverse(nthtail(N * -1, reverse(L)));
drop(_, L)            -> L.


%% @doc Return the first element that satisfies the predicate.
%% @spec find_first(function(), list()) -> {ok, any()} | false

find_first(Pred, L) ->
    find_first(Pred, fun() -> false end, L).

%% @doc Return the value of IfNone() if no element satisfies the predicate.
%% @spec find_first(function(), function(), list()) -> {ok, any()} | any()

find_first(Pred, IfNone, [H|T]) ->
    case Pred(H) of
        true -> {ok, H};
        _    -> find_first(Pred, IfNone, T)
    end;
find_first(_Pred, IfNone, []) ->
    IfNone().


%% @doc Like find_first.

find_last(Pred, L) ->
    find_last(Pred, fun() -> false end, L).

%% @doc Like find_first.

find_last(Pred, IfNone, L) ->
    find_first(Pred, IfNone, reverse(L)).


%% @doc Drop the last element of a list.

init(L) -> reverse(tl(reverse(L))).


%% @doc Find the position of element E in list L.

pos(E, L)                    -> pos(E, L, 1).
pos(_, [], _)                -> 0;
pos(E, [H|_], I) when E == H -> I;
pos(E, [_|T], I)             -> pos(E, T, I+1).


%% @doc Transpose list.

transpose(L) ->
    Len = length(hd(L)),
    transpose1(L, duplicate(Len, [])).
transpose1([R|T], Acc) ->
    NewAcc = zipwith(fun(X, Y) -> Y ++ [X] end, R, Acc),
    transpose1(T, NewAcc);
transpose1([], Acc) ->
    Acc.


%% @doc Remove duplicates from list.

uniq([H|T]) -> [H | uniq([Y || Y <- T, Y =/= H])];
uniq([])    -> [].


%% @doc Shuffle list.

%% Knuth shuffle.
shuffle(L) ->
    N = length(L) * 10000,
    Pairs = map(fun(X) -> {random:uniform(N), X} end, L),
    Sorted = keysort(1, Pairs),
    map(fun({_, X}) -> X end, Sorted).
    

%% @doc Return an element from the list picked randomly.

random(L) ->
    nth(random:uniform(length(L)), L).