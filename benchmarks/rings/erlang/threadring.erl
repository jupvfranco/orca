-module(threadring).
-mode(compile).

% -compile(export_all).
-export([main/1]).

agent(Id) ->
  receive {set_next, Next} -> agent_loop(Next, Id) end.
agent_loop(Next, Id) ->
  receive
    0 -> io:format("~p~n", [Id]), main ! done;
    N -> Next ! N-1 , agent_loop(Next, Id)
  end.

setup_ring_loop(Next, 1) ->
  spawn(fun () -> agent_loop(Next, 1) end);
setup_ring_loop(Next, ActorIndex) ->
  Current = spawn(fun () -> agent_loop(Next, ActorIndex) end),
  setup_ring_loop(Current, ActorIndex-1).

setup_ring(Actors, Msgs) ->
  Last = spawn(fun () -> agent(Actors) end),
  First = setup_ring_loop(Last, Actors-1),
  Last ! {set_next, First},
  First ! Msgs.

reaper(0) ->
  ok;
reaper(Rings) ->
  receive done -> reaper(Rings-1) end.

main(Args) ->
  register(main, self()),
  [Rings, Actors, Msgs] = lists:map(fun erlang:list_to_integer/1, Args),
  [setup_ring(Actors, Msgs) || _ <- lists:seq(1, Rings)],
  reaper(Rings),
  ok.
