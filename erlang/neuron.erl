-module(neuron).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId, Node) ->
  spawn(Node, ?MODULE, loop, [ExoSelf_PId]).

loop(ExoSelf_PId) ->
  receive
    {ExoSelf_PId, {Id, Cx_PId, AF, Input_PIdPs, Output_PIds}} ->
      loop(Id, Cx_PId, AF, {Input_PIdPs, Input_PIdPs}, Output_PIds, 0)
  end.

loop(Id, Cx_PId, AF, {[{Input_PId, Weights}|Input_PIdPs],MInput_PIdPs}, Output_PIds, Acc) ->
  receive
    {Input_PId, forward, Input} ->
      Result = dot(Input, Weights, 0),
      loop(Id, Cx_PId, AF, {Input_PIdPs, MInput_PIdPs}, Output_PIds, Result+Acc);
    {Cx_PId, get_backup} ->
      Cx_PId ! {self(), Id, MInput_PIdPs},
      loop(Id, Cx_PId, AF, {[{Input_PId, Weights} | Input_PIdPs], MInput_PIdPs}, Output_PIds, Acc);
    {Cx_PId, terminate} ->
      ok
  end;

loop(Id, Cx_PId, AF, {[Bias], MInput_PIdPs}, Output_PIds, Acc) ->
  Output = neuron:AF(Acc+Bias),
  [Output_PId ! {self(), forward, [Output]} || Output_PId <- Output_PIds],
  loop(Id, Cx_PId, AF, {MInput_PIdPs, MInput_PIdPs}, Output_PIds, 0);

loop(Id, Cx_PId, AF, {[], MInput_PIdPs}, Output_PIds, Acc) ->
  Output = neuron:AF(Acc),
  [Output_PId ! {self(), forward, [Output]} || Output_PId <- Output_PIds],
  loop(Id, Cx_PId, AF, {MInput_PIdPs, MInput_PIdPs}, Output_PIds, 0).

dot([I|Input], [W|Weights], Acc) ->
  dot(Input, Weights, I*W+Acc);
dot([], [], Acc) ->
  Acc.

tanh(Val) ->
  math:tanh(Val).
