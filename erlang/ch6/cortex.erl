-module(cortex).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId, Node) ->
  spawn(Node, ?MODULE, loop, [ExoSelf_PId]).

loop(ExoSelf_PId) ->
  receive
    {ExoSelf_PId, {Id, SPIds, APIds, NPIds}, TotSteps} ->
      [SPId ! {self(), sync} || SPId <- SPIds],
      loop(Id, ExoSelf_PId, SPIds, {APIds, APIds}, NPIds, TotSteps)
  end.

loop(Id, ExoSelf_PId, SPIds, {_APIds, MAPIds}, NPIds, 0) ->
  io:format("Cortex:~p is backing up and terminating.~n", [Id]),
  Neuron_IdsNWeights = get_backup(NPIds, []),
  ExoSelf_PId ! {self(), backup, Neuron_IdsNWeights},
  [PId ! {self(), terminate} || PId <- SPIds],
  [PId ! {self(), terminate} || PId <- MAPIds],
  [PId ! {self(), terminate} || PId <- NPIds];

loop(Id, ExoSelf_PId, SPIds, {[_APId|APIds], MAPIds}, NPIds, Step) ->
  receive
    {APIds, sync} ->
      loop(Id, ExoSelf_PId, SPIds, {APIds, MAPIds}, NPIds, Step);
    terminate ->
      io:format("Cortex:~p is terminating.~n", [Id]),
      [PId ! {self(), terminate} || PId <- SPIds],
      [PId ! {self(), terminate} || PId <- MAPIds],
      [PId ! {self(), terminate} || PId <- NPIds]
  end;

loop(Id, ExoSelf_PId, SPIds, {[], MAPIds}, NPIds, Step) ->
  [PId ! {self(), sync} || PId <- SPIds],
  loop(Id, ExoSelf_PId, SPIds, {MAPIds, MAPIds}, NPIds, Step-1).

get_backup([NPId|NPIds], Acc) ->
  NPId ! {self(), get_backup},
  receive
    {NPId, NId, WeightTuples} ->
      get_backup(NPIds, [{NId, WeightTuples}|Acc])
  end;
get_backup([], Acc) ->
  Acc.
