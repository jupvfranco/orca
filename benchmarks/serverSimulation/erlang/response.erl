-module(response).
-mode(compile).

% -compile(export_all).
-export([main/1]).

build(1) -> {nil, nil};
build(D) -> {build(D-1), build(D-1)}.

count({nil, nil}) -> 1;
count({L, R}) -> 1 + count(L) + count(R).

reaper(0) ->
  ok;
reaper(NClients) ->
  receive done -> reaper(NClients-1) end.

final(0) ->
  main ! done;
final(NRequests) ->
  receive {done, Id, Count, Start} -> ok end,
  Diff = erlang:monotonic_time() - Start,
  MS = erlang:convert_time_unit(Diff, native, micro_seconds),
  io:format("(~p, ~p, ~p)~n", [Id,Count, MS]),
  final(NRequests-1).

server() ->
  receive {Id, Depth, Final, Start} -> ok end,
  Final ! {done, Id, count(build(Depth)), Start},
  server().

client(Server, Id, NRequests, Final) ->
  [Server ! {Id, Depth, Final, erlang:monotonic_time()}
   || I <- lists:seq(0, NRequests-1), Depth <- [2*I + 6]].

main(Args) ->
  register(main, self()),
  [NServers, NClients, NRequests] =
    lists:map(fun erlang:list_to_integer/1, Args),
  Servers = list_to_tuple(
              [spawn(fun server/0) || _ <- lists:seq(1, NServers)]
             ),
  [spawn(fun() ->client(
                   element((I rem NServers)+1, Servers),
                   I,
                   NRequests,
                   spawn(fun() -> final(NRequests) end))
         end
        )
   || I <- lists:seq(0, NClients-1)],
  reaper(NClients),
  ok.
