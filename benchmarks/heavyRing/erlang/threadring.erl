-module(threadring).
-mode(compile).
 
-compile(export_all).
% -export([main/1]).
 
build_tree(1) -> {nil, nil};
build_tree(D) -> {build_tree(D-1), build_tree(D-1)}.
 
agent(Id) ->
  receive {set_next, Next} -> agent_loop(Next, Id) end.
agent_loop(Next, Id) ->
  receive
    {0, _} -> io:format("~p~n", [Id]), main ! done;
    {N, G} -> Next ! {N-1, G} , agent_loop(Next, Id)
  end.
 
setup_ring_loop(Next, 1) ->
  spawn(fun () -> agent_loop(Next, 1) end);
setup_ring_loop(Next, ActorIndex) ->
  Current = spawn(fun () -> agent_loop(Next, ActorIndex) end),
  setup_ring_loop(Current, ActorIndex-1).
 
setup_ring(Depth, Actors, Msgs) ->
  Last = spawn(fun () -> agent(Actors) end),
  First = setup_ring_loop(Last, Actors-1),
  Last ! {set_next, First},
  First ! {Msgs, build_tree(Depth)}.
 
reaper(0) ->
  ok;
reaper(Rings) ->
  receive done -> reaper(Rings-1) end.
 
main(Args) ->
  register(main, self()),
  [Actors, Depth, Rounds] = lists:map(fun erlang:list_to_integer/1, Args),
  Msgs = Actors * Rounds,
  setup_ring(Depth, Actors, Msgs),
  reaper(1),
  ok.

