-module(actuator).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId, Node) ->
  spawn(Node, ?MODULE, loop, [ExoSelf_PId]).

loop(ExoSelf_PId) ->
  receive
    {ExoSelf_PId, {Id, Cx_PId, ActuatorName, Fanin_PIds}} ->
      loop(Id, Cx_PId, ActuatorName, {Fanin_PIds, Fanin_PIds}, [])
  end.

loop(Id, Cx_PId, AName, {[From_PId|Fanin_PIds], MFanin_PIds}, Acc) ->
  receive
    {From_PId, forward, Input} ->
      loop(Id, Cx_PId, AName, {Fanin_PIds, MFanin_PIds}, lists:append(Input, Acc));
    {Cx_PId, terminate} ->
      ok
  end;

loop(Id, Cx_PId, AName, {[],MFanin_PIds}, Acc) ->
  actuator:AName(lists:reverse(Acc)),
  Cx_PId ! {self(), sync},
  loop(Id, Cx_PId, AName, {MFanin_PIds,MFanin_PIds},[]).

pts(Result) ->
  io:format("actuator:pts(Results):~p~n", [Result]).
