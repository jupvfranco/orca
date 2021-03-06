% The Computer Language Benchmarks Game
% http://benchmarksgame.alioth.debian.org/
%
% contributed by Isaac Gouy (Erlang novice)
% parallelized by Kevin Scaldeferri

-module(binarytrees).
-export([main/1]).
-export([depth/2]).

-define(Min,4).

main([Arg]) ->
   N = list_to_integer(Arg),
   Max = lists:max([?Min+2,N]),

   Stretch = Max + 1,
   io:fwrite("stretch tree of depth ~w\t check: ~w~n",
      [ Stretch, itemCheck(bottomUp(0,Stretch)) ]),

   LongLivedTree = bottomUp(0,Max),
   depthLoop(?Min,Max),

   io:fwrite("long lived tree of depth ~w\t check: ~w~n",
      [ Max, itemCheck(LongLivedTree) ]),

   halt(0).

pmap_depth(Arg2, List) ->
  Parent = self(),
  Children =
    [spawn(fun () -> Parent ! {self(), depth(Arg1, Arg2)} end)
     || Arg1 <- List],
  [receive {ChildId, Ret} -> Ret end || ChildId <- Children].

repeat_to_length(List, N) when length(List) >= N ->
  lists:sublist(List, N);
repeat_to_length(List, N) ->
  repeat_to_length(List ++ List, N).

depthLoop(D,M) ->
    % Results = rpc:pmap({?MODULE, depth}, [M], lists:seq(D, M, 2)),
    Results = pmap_depth(M, lists:seq(D, M, 2)),
    % Results = pmap_depth(M, repeat_to_length(lists:seq(D, M, 2), 128)),
    lists:foreach(fun(Result) ->
                          io:fwrite("~w\t trees of depth ~w\t check: ~w~n", Result)
                  end,
                  Results).

depth(D,M) ->
    N = 1 bsl (M-D + ?Min),
    [ 2*N, D, sumLoop(N,D,0) ].

sumLoop(0,_,Sum) -> Sum;
sumLoop(N,D,Sum) ->
   sumLoop(N-1,D, Sum + itemCheck(bottomUp(N,D)) + itemCheck(bottomUp(-1*N,D))).

bottomUp(I,0) -> {I, nil, nil};
bottomUp(I,D) -> {I, bottomUp(2*I-1,D-1), bottomUp(2*I,D-1)}.

itemCheck(nil) -> 0;
itemCheck({I,Left,Right}) ->
   I + itemCheck(Left) - itemCheck(Right).
